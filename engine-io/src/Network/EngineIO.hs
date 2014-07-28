{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.EngineIO
  ( -- $intro

    -- * Example Usage
    -- $example

    -- * Running Engine.IO applications
    initialize
  , handler
  , EngineIO
  , ServerAPI (..)
  , SocketApp(..)

    -- * Interacting with 'Socket's
  , send
  , receive
  , Socket
  , SocketId
  , socketId
  , getOpenSockets
  , dupRawReader

    -- * The Engine.IO Protocol
    -- This section of the API is somewhat low-level, and exposes the raw
    -- protocol to users.

    -- ** Packets
  , Packet(..)
  , parsePacket
  , encodePacket
  , PacketType

    -- ** Packet Contents
  , PacketContent(..)

    -- ** Payloads
  , Payload(..)
  , parsePayload
  , encodePayload

    -- ** Transport types
  , TransportType(..)
  , parseTransportType
  ) where

import Prelude hiding (any)

import Control.Applicative
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException(SomeException), try)
import Control.Monad (MonadPlus, forever, guard, mzero, replicateM)
import Control.Monad.Trans.Iter (cutoff, delay, retract)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (unfoldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (eitherT, left)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson ((.=))
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (asum, for_)
import Data.Function (fix, on)
import Data.Ix (inRange)
import Data.List (foldl')
import Data.Monoid ((<>), mconcat, mempty)
import Data.Ord (comparing)
import Data.Traversable (for)

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.Delay as STMDelay
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as AttoparsecC8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified Network.WebSockets as WebSockets
import qualified Network.WebSockets.Connection as WebSockets
import qualified System.Random.MWC as Random

--------------------------------------------------------------------------------
-- | The possible packet types, as mentioned in the
-- <https://github.com/Automattic/engine.io-protocol Engine.IO protocol documentation>
data PacketType = Open | Close | Ping | Pong | Message | Upgrade | Noop
  deriving (Bounded, Enum, Eq, Read, Show)


--------------------------------------------------------------------------------
packetTypeToIndex :: Num i => PacketType -> i
packetTypeToIndex t =
  case t of
    Open -> 0
    Close -> 1
    Ping -> 2
    Pong -> 3
    Message -> 4
    Upgrade -> 5
    Noop -> 6
{-# INLINE packetTypeToIndex #-}


--------------------------------------------------------------------------------
packetTypeFromIndex :: (Eq i, MonadPlus m, Num i) => i -> m PacketType
packetTypeFromIndex i =
  case i of
    0 -> return Open
    1 -> return Close
    2 -> return Ping
    3 -> return Pong
    4 -> return Message
    5 -> return Upgrade
    6 -> return Noop
    _ -> mzero
{-# INLINE packetTypeFromIndex #-}


--------------------------------------------------------------------------------
-- | A single Engine.IO packet.
data Packet = Packet !PacketType !PacketContent
  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | The contents attached to a packet. Engine.IO makes a clear distinction
-- between binary data and text data. Clients will receive binary data as a
-- Javascript @ArrayBuffer@, where as 'TextPacket's will be received as UTF-8
-- strings.
data PacketContent
  = BinaryPacket !BS.ByteString
  | TextPacket !Text.Text
  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | Parse bytes as an 'Packet' assuming the packet contents extends to the
-- end-of-input.
parsePacket :: Attoparsec.Parser Packet
parsePacket = parsePacket' Attoparsec.takeByteString
{-# INLINE parsePacket #-}


--------------------------------------------------------------------------------
-- | Parse a 'Packet', nested another parser for the body of the packet.
parsePacket' :: Attoparsec.Parser BS.ByteString -> Attoparsec.Parser Packet
parsePacket' body = parseBase64 <|> parseBinary <|> parseText

  where
  parseBase64 = do
    _ <- AttoparsecC8.char 'b'
    Packet <$> c8PacketType
           <*> (either fail (return . BinaryPacket) . Base64.decode =<< body)

  parseBinary = do
    Packet <$> (packetTypeFromIndex =<< Attoparsec.satisfy (inRange (0, 6)))
           <*> (BinaryPacket <$> body)

  parseText = do
    Packet <$> c8PacketType
           <*> (TextPacket . Text.decodeUtf8 <$> body)

  c8PacketType =
    packetTypeFromIndex . digitToInt  =<< AttoparsecC8.satisfy (inRange ('0', '6'))

{-# INLINE parsePacket' #-}


--------------------------------------------------------------------------------
-- | Encode a 'Packet' to a 'Builder.Builder'. The first argument determines
-- whether or not binary is supported - if not, binary data will be base 64
-- encoded.
encodePacket
  :: Bool
  -- ^ If true, all bytes can be used. Otherwise, the packet will be base 64
  -- encoded.
  -> Packet
  -> Builder.Builder
encodePacket True (Packet t (BinaryPacket bytes)) =
  Builder.word8 (packetTypeToIndex t) <>
    Builder.byteString bytes

encodePacket False (Packet t (BinaryPacket bytes)) =
  Builder.char8 'b' <>
    Builder.char8 (intToDigit (packetTypeToIndex t)) <>
      Builder.byteString (Base64.encode bytes)

encodePacket _ (Packet t (TextPacket bytes)) =
  Builder.char8 (intToDigit (packetTypeToIndex t)) <>
    Builder.byteString (Text.encodeUtf8 bytes)


--------------------------------------------------------------------------------
-- | A 'Payload' is a stream of 0-or-more 'Packet's.
newtype Payload = Payload (V.Vector Packet)
  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | Parse a stream of bytes into a 'Payload'.
parsePayload :: Attoparsec.Parser Payload
parsePayload = Payload <$> go
  where
  go = do
    _ <- Attoparsec.satisfy (`elem` [0, 1])
    len <- parseLength =<< Attoparsec.many1 (Attoparsec.satisfy (inRange (0, 9)))
    _ <- Attoparsec.word8 maxBound

    packet <- parsePacket' (Attoparsec.take (len - 1)) -- the type consumes 1 byte
    (V.singleton packet <$ Attoparsec.endOfInput) <|> (V.cons packet <$> go)

  parseLength bytes = do
    guard (length bytes <= 319)
    return $ foldl' (\n x -> n * 10 + x) 0 $ map fromIntegral bytes


--------------------------------------------------------------------------------
-- | Encode a 'Payload' to a 'Builder.Builder'. As with 'encodePacket', the
-- first argument determines whether or not binary transmission is supported.
encodePayload
  :: Bool
  -- ^ If true, all bytes can be used. Otherwise, the packet will be base 64
  -- encoded.
  -> Payload
  -> Builder.Builder
encodePayload supportsBinary (Payload packets) =
  let encodeOne packet =
        let bytes = encodePacket supportsBinary packet
        in mconcat [ Builder.word8 $ if isBinaryPacket packet then 1 else 0
                   , mconcat $ map (Builder.word8 . read . pure) $
                       show (LBS.length (Builder.toLazyByteString bytes))
                   , Builder.word8 maxBound
                   , bytes
                   ]

  in V.foldl' (\bytes p -> bytes <> encodeOne p) mempty packets

  where
  isBinaryPacket (Packet _ (BinaryPacket _)) = True
  isBinaryPacket _ = False


--------------------------------------------------------------------------------
-- | The possible types of transports Engine.IO supports.
data TransportType
  = Polling
    -- ^ XHR long polling.
  | Websocket
    -- ^ HTML 5 websockets.
  deriving (Eq, Show)

instance Aeson.ToJSON TransportType where
  toJSON t = Aeson.toJSON $ (`asTypeOf` show t) $
    case t of
      Polling -> "polling"
      Websocket -> "websocket"


--------------------------------------------------------------------------------
-- | Attempt to parse a 'TransportType' from its textual representation.
parseTransportType :: Text.Text -> Maybe TransportType
parseTransportType t =
  case t of
    "polling" -> Just Polling
    "websocket" -> Just Websocket
    _ -> Nothing
{-# INLINE parseTransportType #-}


--------------------------------------------------------------------------------
-- | The type of unique Engine.IO sessions. This is currently a base64-encoded
-- random identifier.
type SocketId = BS.ByteString


--------------------------------------------------------------------------------
data Transport = Transport
  { transIn :: STM.TChan Packet
  , transOut :: STM.TChan Packet
  , transType :: !TransportType
  }


--------------------------------------------------------------------------------
-- | A connected Engine.IO session.
data Socket = Socket
  { socketId :: !SocketId
  , socketTransport :: STM.TVar Transport
  , socketIncomingMessages :: STM.TChan PacketContent
  , socketOutgoingMessages :: STM.TChan PacketContent
  , socketRawIncomingBroadcast :: STM.TChan Packet
  }

instance Eq Socket where
  (==) = (==) `on` socketId

instance Ord Socket where
  compare = comparing socketId


--------------------------------------------------------------------------------
-- | Receive data from the client, blocking if the input queue is empty.
receive :: Socket -> STM.STM PacketContent
receive Socket{..} = STM.readTChan socketIncomingMessages
{-# INLINE receive #-}


--------------------------------------------------------------------------------
-- | Send a packet to the client. This is a non-blocking write.
send :: Socket -> PacketContent -> STM.STM ()
send Socket{..} = STM.writeTChan socketOutgoingMessages
{-# INLINE send #-}


--------------------------------------------------------------------------------
-- | A dictionary of functions that Engine.IO needs in order to provide
-- communication channels.
data ServerAPI m = ServerAPI
  { srvGetQueryParams :: m (HashMap.HashMap BS.ByteString [BS.ByteString])
    -- ^ Retrieve the 'HashMap.HashMap' of query parameters in the request path
    -- to their zero-or-more values.

  , srvTerminateWithResponse :: Int -> BS.ByteString -> Builder.Builder -> forall a . m a
    -- ^ Send a response with the given status code, content type and body. This
    -- should also terminate the web request entirely, such that further actions
    -- in @m@ have no effect.

  , srvParseRequestBody :: forall a. Attoparsec.Parser a -> m (Either String a)
    -- ^ Run a 'Attoparsec.Parser' against the request body.

  , srvGetRequestMethod :: m BS.ByteString
    -- ^ Get the request method of the current request. The request method
    -- should be in uppercase for standard methods (e.g., @GET@).

  , srvRunWebSocket :: WebSockets.ServerApp -> m ()
    -- ^ Upgrade the current connection to run a WebSocket action.
  }


--------------------------------------------------------------------------------
-- | An opaque data type representing an open Engine.IO server.
data EngineIO = EngineIO
  { eioOpenSessions :: STM.TVar (HashMap.HashMap SocketId Socket)
  , eioRng :: MVar Random.GenIO
  }


--------------------------------------------------------------------------------
-- | 'initialize' initializes a new Engine.IO server. You can later serve this
-- session by using 'handler'.
initialize :: IO EngineIO
initialize =
  EngineIO
    <$> STM.newTVarIO mempty
    <*> (Random.createSystemRandom >>= newMVar)


--------------------------------------------------------------------------------
-- | Retrieve a list of /all/ currently open Engine.IO sessions.
getOpenSockets :: EngineIO -> STM.STM (HashMap.HashMap SocketId Socket)
getOpenSockets = STM.readTVar . eioOpenSessions


--------------------------------------------------------------------------------
data EngineIOError = BadRequest | TransportUnknown | SessionIdUnknown
  deriving (Bounded, Enum, Eq, Show)


--------------------------------------------------------------------------------
-- | The application to run for the duration of a connected socket.
data SocketApp = SocketApp
  { saApp :: IO ()
    -- ^ An IO action to run for the duration of the socket's lifetime. If this
    -- action terminates, the connection will be closed. You will likely want
    -- to loop 'Control.Monad.forever' and block as appropriate with 'receive'.

  , saOnDisconnect :: IO ()
    -- ^ An action to execute when the connection is closed, either by 'saApp'
    -- terminating, or the client disconnecting.
  }


--------------------------------------------------------------------------------
{-|

Build the necessary handler for Engine.IO. The result of this function is a
computation that you should serve under the @/engine.io/@ path.

'handler' takes a function as an argument that is called every time a new
session is created. This function runs in the @m@ monad, so you have access to
initial web request, which may be useful for performing authentication or
collecting cookies. This function then returns a 'ServerApp', describing the
main loop and an action to perform on socket disconnection.

-}
handler :: MonadIO m => EngineIO -> (Socket -> m SocketApp) -> ServerAPI m -> m ()
handler eio socketHandler api@ServerAPI{..} = do
  queryParams <- srvGetQueryParams
  eitherT (serveError api) return $ do
    reqTransport <- maybe (left TransportUnknown) return $ do
      [t] <- HashMap.lookup "transport" queryParams
      parseTransportType (Text.decodeUtf8 t)

    socket <-
      for (HashMap.lookup "sid" queryParams) $ \sids -> do
        sid <- case sids of
                 [sid] -> return sid
                 _ -> left SessionIdUnknown

        mSocket <- liftIO (STM.atomically (HashMap.lookup sid <$> getOpenSockets eio))
        case mSocket of
          Nothing -> left SessionIdUnknown
          Just s -> return s

    supportsBinary <-
      case HashMap.lookup "b64" queryParams of
        Just ["1"] -> return False
        Just ["0"] -> return True
        Nothing    -> return True
        _          -> left BadRequest

    case socket of
      Just s -> do
        transport <- liftIO $ STM.atomically $ STM.readTVar (socketTransport s)
        case transType transport of
          Polling
            | reqTransport == Polling -> lift (handlePoll api transport supportsBinary)
            | reqTransport == Websocket -> lift (upgrade api s)

          _ -> left BadRequest

      Nothing ->
        lift (freshSession eio socketHandler api supportsBinary)


--------------------------------------------------------------------------------
freshSession
  :: MonadIO m
  => EngineIO
  -> (Socket -> m SocketApp)
  -> ServerAPI m
  -> Bool
  -> m ()
freshSession eio socketHandler api supportsBinary = do
  socket <- do
    mkSocket <- liftIO $ do
      transport <- STM.newTVarIO =<< (Transport <$> STM.newTChanIO <*> STM.newTChanIO <*> pure Polling)
      incoming <- STM.newTChanIO
      outgoing <- STM.newTChanIO
      rawInBroadcast <- STM.newBroadcastTChanIO
      return (\sId -> Socket sId transport incoming outgoing rawInBroadcast)

    let
      tryAllocation = liftIO $ do
        sId <- newSocketId eio
        STM.atomically $ runMaybeT $ do
          openSessions <- lift (STM.readTVar (eioOpenSessions eio))
          guard (not (HashMap.member sId openSessions))
          let socket = mkSocket sId
          lift (STM.modifyTVar' (eioOpenSessions eio) (HashMap.insert sId socket))
          return socket

      untilSuccess f = maybe (delay (untilSuccess f)) return =<< f

    maybeSocket <- retract (cutoff 10 (untilSuccess tryAllocation))
    maybe (srvTerminateWithResponse api 500 "text/plain" "Session allocation failed")
          return maybeSocket

  app <- socketHandler socket
  userSpace <- liftIO $ Async.async (saApp app)

  pingTimeoutDelay <- liftIO $ STMDelay.newDelay (pingTimeout * 1000000)
  heartbeat <- liftIO $ Async.async $
    STM.atomically (STMDelay.waitDelay pingTimeoutDelay)

  brain <- liftIO $ Async.async $ fix $ \loop -> do
    mMessage <- STM.atomically $ do
      transport <- STM.readTVar (socketTransport socket)
      asum
        [ do req <- STM.readTChan (transIn transport)
             case req of
               Packet Message m ->
                 STM.writeTChan (socketIncomingMessages socket) m

               Packet Ping m ->
                 STM.writeTChan (transOut transport) (Packet Pong m)

               _ ->
                 return ()

             STM.writeTChan (socketRawIncomingBroadcast socket) req
             return (Just req)

        , do STM.readTChan (socketOutgoingMessages socket)
               >>= STM.writeTChan (transOut transport) . Packet Message

             return Nothing
        ]

    case mMessage of
      Just (Packet Close _) ->
        return ()

      _ -> do
        STMDelay.updateDelay pingTimeoutDelay (pingTimeout * 1000000)
        loop

  _ <- liftIO $ Async.async $ do
    _ <- Async.waitAnyCatchCancel [ userSpace, brain, heartbeat ]
    STM.atomically (STM.modifyTVar' (eioOpenSessions eio) (HashMap.delete (socketId socket)))
    saOnDisconnect app

  let openMessage = OpenMessage { omSocketId = socketId socket
                                , omUpgrades = [ Websocket ]
                                , omPingTimeout = pingTimeout * 1000
                                , omPingInterval = 25000
                                }

      payload = Payload $ V.singleton $
                  Packet Open (TextPacket $ Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode openMessage)

  writeBytes api (encodePayload supportsBinary payload)

  where

  pingTimeout = 60

--------------------------------------------------------------------------------
upgrade :: MonadIO m => ServerAPI m -> Socket -> m ()
upgrade ServerAPI{..} socket = srvRunWebSocket go

  where

  go pending = do
    conn <- WebSockets.acceptRequest $
      -- We do our ping/pong, so disable `websockets` doing this.
      pending { WebSockets.pendingOnAccept = (const $ return ()) }

    mWsTransport <- runMaybeT $ do
      Packet Ping (TextPacket "probe") <- lift (receivePacket conn)
      lift (sendPacket conn (Packet Pong (TextPacket "probe")))

      (wsIn, wsOut) <- liftIO $ STM.atomically $ do
        currentTransport <- STM.readTVar (socketTransport socket)
        STM.writeTChan (transOut currentTransport) (Packet Noop (TextPacket Text.empty))
        return (transIn currentTransport, transOut currentTransport)

      Packet Upgrade body <- lift (receivePacket conn)
      guard (body == TextPacket Text.empty || body == BinaryPacket BS.empty)

      return (Transport wsIn wsOut Websocket)

    for_ mWsTransport $ \wsTransport@Transport { transIn = wsIn, transOut = wsOut } -> do
      -- The client has completed the upgrade, so we can swap out the current
      -- transport with a WebSocket transport.
      STM.atomically (STM.writeTVar (socketTransport socket) wsTransport)

      reader <- Async.async $ forever $ do
        p <- STM.atomically (STM.readTChan wsOut)
        sendPacket conn p

      fix $ \loop -> do
        e <- try (receivePacket conn >>= STM.atomically . STM.writeTChan wsIn)
        case e of
          Left (SomeException _) ->
            return ()

          Right _ -> loop

      Async.cancel reader
      STM.atomically (STM.writeTChan wsIn (Packet Close (TextPacket Text.empty)))

  receivePacket conn = do
    msg <- WebSockets.receiveDataMessage conn
    case msg of
      WebSockets.Text bytes ->
        case Attoparsec.parseOnly parsePacket (LBS.toStrict bytes)  of
          Left ex -> do
            putStrLn $ "Malformed packet received: " ++ show bytes ++ " (" ++ show ex ++ ")"
            receivePacket conn

          Right p -> return p

      other -> do
        putStrLn $ "Unknown WebSocket message: " ++ show other
        receivePacket conn

  sendPacket conn (Packet t (TextPacket text)) =
    WebSockets.sendTextData conn $
      Text.encodeUtf8 $ Text.pack $ BSChar8.unpack $ Text.encodeUtf8 $
        Text.pack (pure $ intToDigit (packetTypeToIndex t)) <> text

  sendPacket conn p@(Packet _ (BinaryPacket _)) = do
    WebSockets.sendBinaryData conn (Builder.toLazyByteString (encodePacket True p))


--------------------------------------------------------------------------------
handlePoll :: MonadIO m => ServerAPI m -> Transport -> Bool -> m ()
handlePoll api@ServerAPI{..} transport supportsBinary = do
  requestMethod <- srvGetRequestMethod
  case requestMethod of
    m | m == "GET" -> poll
    m | m == "POST" -> post
    _ -> serveError api BadRequest

  where

  poll = do
    readTimeout <- liftIO $ STM.registerDelay (45 * 1000000)

    let out = transOut transport

    -- Here we attempt to read as much from the transport output as we can.
    -- We also consider the timeout above, such that if we haven't even read
    -- one message by the timeout is reached, we instead emit a `ping`.
    packets <- liftIO $ do
      p <- STM.atomically $ do
        let dequeueHead = Just <$> STM.readTChan out
            timeout = Nothing <$ (STM.readTVar readTimeout >>= STM.check)

        dequeueHead <|> timeout

      case p of
        Just p' ->
          (p' :) <$> unfoldM (STM.atomically (STM.tryReadTChan (transOut transport)))

        Nothing ->
          return [ Packet Ping (BinaryPacket mempty) ]

    writeBytes api (encodePayload supportsBinary (Payload (V.fromList packets)))

  post = do
    p <- srvParseRequestBody parsePayload
    case p of
      Left ex -> do
        liftIO $ putStrLn $ "WARNING: Parse failure in Network.EngineIO.handlePoll: " ++ show ex
        srvTerminateWithResponse 400 "text/plain" "Empty request body"

      Right (Payload packets) ->
        liftIO $ STM.atomically (V.mapM_ (STM.writeTChan (transIn transport)) packets)


--------------------------------------------------------------------------------
writeBytes :: Monad m => ServerAPI m -> Builder.Builder -> m a
writeBytes ServerAPI {..} builder = do
  srvTerminateWithResponse 200 "application/octet-stream" builder
{-# INLINE writeBytes #-}


--------------------------------------------------------------------------------
newSocketId :: EngineIO -> IO SocketId
newSocketId eio =
  Base64.encode . BS.pack
    <$> withMVar (eioRng eio) (replicateM 15 . Random.uniformR (0, 63))
{-# INLINE newSocketId #-}


--------------------------------------------------------------------------------
data OpenMessage = OpenMessage
  { omSocketId :: !SocketId
  , omUpgrades :: [TransportType]
  , omPingTimeout :: !Int
  , omPingInterval :: !Int
  }

instance Aeson.ToJSON OpenMessage where
  toJSON OpenMessage {..} = Aeson.object
    [ "sid" .= Text.decodeUtf8 omSocketId
    , "upgrades" .= omUpgrades
    , "pingTimeout" .= omPingTimeout
    , "pingInterval" .= omPingInterval
    ]


--------------------------------------------------------------------------------
serveError :: Monad m => ServerAPI m -> EngineIOError -> m a
serveError ServerAPI{..} e = srvTerminateWithResponse 400 "application/json" $
  Builder.lazyByteString $ Aeson.encode $ Aeson.object
    [ "code" .= errorCode, "message" .= errorMessage ]

  where
  errorCode :: Int
  errorCode = case e of
                TransportUnknown -> 0
                SessionIdUnknown -> 1
                BadRequest -> 3

  errorMessage :: Text.Text
  errorMessage = case e of
                   TransportUnknown -> "Transport unknown"
                   SessionIdUnknown -> "Session ID unknown"
                   BadRequest -> "Bad request"


--------------------------------------------------------------------------------
-- | Create a new 'IO' action to read the socket's raw incoming communications.
-- The result of this call is iteslf an STM action, which will called will return
-- the next unread incoming packet (or block). This provides you with a separate
-- channel to monitor incoming communications. This may useful to monitor to
-- determine if the socket has activity.
--
-- This is a fairly low level operation, so you will receive *all* packets -
-- including pings and other control codes.
dupRawReader :: Socket -> IO (STM.STM Packet)
dupRawReader s = do
  c <- STM.atomically (STM.dupTChan (socketRawIncomingBroadcast s))
  return (STM.readTChan c)


{- $intro

@Network.EngineIO@ is a Haskell of implementation of
<https://github.com/automattic/engine.io Engine.IO>, a realtime framework for
the web. Engine.IO provides you with an abstraction for doing real-time
communication between a server and a client. Engine.IO abstracts the framing and
transport away, so that you can have real-time communication over long-polling
HTTP requests, which are later upgraded to web sockets, if available.

@Network.EngineIO@ needs to be provided with a 'ServerAPI' in order to be
ran. 'ServerAPI' informs us how to fetch request headers, write HTTP responses
to the client, and run web socket applications. Hackage contains implementations
of 'ServerAPI' as:

* <http://hackage.haskell.org/package/engine-io-snap engine-io-snap> for Snap.

If you write your own implementation of 'ServerAPI', please share it on Hackage
and I will link to it from here.

-}

{- $example

A simple echo server is easy to write with Engine.IO. The following imports will
be required:

> import Control.Concurrent.STM
> import Control.Monad (forever)
> import Network.EngineIO
> import Network.EngineIO.Snap
> import Snap.Http.Server

Next, we write the implementation of our per-socket processing logic. For this
application we simply receive from the socket, and then send the result back to
the socket. We wrap this all in 'Control.Monad.forever' as this connection
should never terminate.

> handleSocket :: Socket -> IO ()
> handleSocket s = forever $ atomically $
>   receive s >>= send s

Finally, we add a @main@ function to our application to launch it. I'll use
@engine-io-snap@ as my server implementation:

> main :: IO ()
> main = do
>   eio <- initialize
>   quickHttpServe $ handler eio handleSocket

This means that /any/ URL works as the Engine.IO server, which is sufficient for
our example. In a real production application, you will probably want to nest
the 'handler' under @/engine.io@.

-}
