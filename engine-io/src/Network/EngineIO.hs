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

    -- * The Engine.IO Protocol
    -- ** Packets
  , Packet(..)
  , parsePacket
  , encodePacket

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
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as AttoparsecC8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified Network.WebSockets as WebSockets
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
data Packet = Packet !PacketType !PacketContent
  deriving (Eq, Show)

data PacketContent
  = BinaryPacket !BS.ByteString
  | TextPacket !Text.Text
  deriving (Eq, Show)


--------------------------------------------------------------------------------
parsePacket :: Attoparsec.Parser Packet
parsePacket = parsePacket' Attoparsec.takeByteString
{-# INLINE parsePacket #-}


--------------------------------------------------------------------------------
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
encodePacket :: Bool -> Packet -> Builder.Builder
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
newtype Payload = Payload (V.Vector Packet)
  deriving (Eq, Show)


--------------------------------------------------------------------------------
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
encodePayload :: Bool -> Payload -> Builder.Builder
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
data TransportType = Polling | Websocket
  deriving (Eq, Show)

instance Aeson.ToJSON TransportType where
  toJSON t = Aeson.toJSON $ (`asTypeOf` show t) $
    case t of
      Polling -> "polling"
      Websocket -> "websocket"


--------------------------------------------------------------------------------
parseTransportType :: Text.Text -> Maybe TransportType
parseTransportType t =
  case t of
    "polling" -> Just Polling
    "websocket" -> Just Websocket
    _ -> Nothing
{-# INLINE parseTransportType #-}


--------------------------------------------------------------------------------
type SocketId = BS.ByteString


--------------------------------------------------------------------------------
data Transport = Transport
  { transIn :: STM.TChan Packet
  , transOut :: STM.TChan Packet
  , transType :: !TransportType
  }


--------------------------------------------------------------------------------
data Socket = Socket
  { socketId :: !SocketId
  , socketTransport :: STM.TVar Transport
  , socketIncomingMessages :: STM.TChan PacketContent
  , socketOutgoingMessages :: STM.TChan PacketContent
  }

instance Eq Socket where
  (==) = (==) `on` socketId

instance Ord Socket where
  compare = comparing socketId


--------------------------------------------------------------------------------
receive :: Socket -> STM.STM PacketContent
receive Socket{..} = STM.readTChan socketIncomingMessages
{-# INLINE receive #-}


--------------------------------------------------------------------------------
send :: Socket -> PacketContent -> STM.STM ()
send Socket{..} = STM.writeTChan socketOutgoingMessages
{-# INLINE send #-}


--------------------------------------------------------------------------------
data ServerAPI m = ServerAPI
  { srvGetQueryParams :: m (HashMap.HashMap BS.ByteString [BS.ByteString])
  , srvWriteBuilder :: Builder.Builder -> m ()
  , srvSetContentType :: BS.ByteString -> m ()
  , srvParseRequestBody :: forall a. Attoparsec.Parser a -> m a
  , srvGetRequestMethod :: m BS.ByteString
  , srvRunWebSocket :: WebSockets.ServerApp -> m ()
  , srvSetResponseCode :: Int -> m ()
  }


--------------------------------------------------------------------------------
data EngineIO = EngineIO
  { eioOpenSessions :: STM.TVar (HashMap.HashMap SocketId Socket)
  , eioRng :: MVar Random.GenIO
  }


--------------------------------------------------------------------------------
initialize :: IO EngineIO
initialize =
  EngineIO
    <$> STM.newTVarIO mempty
    <*> (Random.createSystemRandom >>= newMVar)


--------------------------------------------------------------------------------
getOpenSockets :: EngineIO -> STM.STM (HashMap.HashMap SocketId Socket)
getOpenSockets = STM.readTVar . eioOpenSessions


--------------------------------------------------------------------------------
data EngineIOError = BadRequest | TransportUnknown | SessionIdUnknown
  deriving (Bounded, Enum, Eq, Show)


--------------------------------------------------------------------------------
data SocketApp = SocketApp
  { saApp :: IO ()
  , saOnDisconnect :: IO ()
  }


--------------------------------------------------------------------------------
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
  sId <- liftIO (newSocketId eio)

  socket <- liftIO $
    Socket <$> pure sId
           <*> (do transport <- Transport <$> STM.newTChanIO
                                          <*> STM.newTChanIO
                                          <*> pure Polling
                   STM.newTVarIO transport)
           <*> STM.newTChanIO
           <*> STM.newTChanIO

  liftIO $ STM.atomically (STM.modifyTVar' (eioOpenSessions eio) (HashMap.insert sId socket))

  app <- socketHandler socket
  userSpace <- liftIO $ Async.async (saApp app)

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

             return (Just req)

        , do STM.readTChan (socketOutgoingMessages socket)
               >>= STM.writeTChan (transOut transport) . Packet Message

             return Nothing
        ]

    case mMessage of
      Just (Packet Close _) -> return ()
      _ -> loop

  _ <- liftIO $ Async.async $ do
    _ <- Async.waitAnyCatchCancel [ userSpace, brain ]
    STM.atomically (STM.modifyTVar' (eioOpenSessions eio) (HashMap.delete sId))
    saOnDisconnect app

  let openMessage = OpenMessage { omSocketId = sId
                                , omUpgrades = [ Websocket ]
                                , omPingTimeout = 60000
                                , omPingInterval = 25000
                                }

      payload = Payload $ V.singleton $
                  Packet Open (TextPacket $ Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode openMessage)

  writeBytes api (encodePayload supportsBinary payload)


--------------------------------------------------------------------------------
upgrade :: MonadIO m => ServerAPI m -> Socket -> m ()
upgrade ServerAPI{..} socket = srvRunWebSocket go

  where

  go pending = do
    conn <- WebSockets.acceptRequest pending

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

  sendPacket conn p@(Packet _ (TextPacket _)) = do
    WebSockets.sendTextData conn (Builder.toLazyByteString (encodePacket False p))

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
    let out = transOut transport
    packets <- liftIO $
      (:) <$> STM.atomically (STM.readTChan out)
          <*> unfoldM (STM.atomically (STM.tryReadTChan (transOut transport)))

    writeBytes api (encodePayload supportsBinary (Payload (V.fromList packets)))

  post = do
    Payload packets <- srvParseRequestBody parsePayload
    liftIO $ STM.atomically (V.mapM_ (STM.writeTChan (transIn transport)) packets)


--------------------------------------------------------------------------------
writeBytes :: Monad m => ServerAPI m -> Builder.Builder -> m ()
writeBytes ServerAPI {..} builder = do
  srvWriteBuilder builder
  srvSetContentType "application/octet-stream"
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
serveError :: Monad m => ServerAPI m -> EngineIOError -> m ()
serveError ServerAPI{..} e = do
  srvSetResponseCode 400
  srvSetContentType "application/json"
  srvWriteBuilder $ Builder.lazyByteString $ Aeson.encode $ Aeson.object
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


{- $intro

'Network.EngineIO' is a Haskell of implementation of
<https://github.com/automattic/engine.io Engine.IO>, a realtime framework for
the web. Engine.IO provides you with an abstraction for doing real-time
communication between a server and a client. Engine.IO abstracts the framing and
transport away, so that you can have real-time communication over long-polling
HTTP requests, which are later upgraded to web sockets, if available.

'Network.EngineIO' needs to be provided with a 'ServerAPI' in order to be
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
