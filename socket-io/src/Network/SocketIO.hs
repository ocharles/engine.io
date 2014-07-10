{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.SocketIO
  ( -- * Packet Types
    PacketType(..)
  , parsePacketType
  , encodePacketType

    -- * Packets
  , Packet(..)
  , parsePacket
  , encodePacket

    -- * Running Socket.IO Applications
  , initialize
  , EventHandler

  , on
  , on_
  , onJSON

  , appendDisconnectHandler

  , emit
  , emitJSON
  , emitTo
  , emitJSONTo

  , broadcast
  , broadcastJSON

    -- * Sockets
  , Socket
  , socketId

    -- * RoutingTable
  , RoutingTable
  ) where

import Control.Applicative
import Control.Monad (forever, guard, mzero, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.State (MonadState, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, execStateT)
import Data.Char (isDigit)
import Data.Foldable (asum, forM_)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mempty)
import Data.Ord (comparing)

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as AttoparsecC8
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Function as Function
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified Network.EngineIO as EIO

--------------------------------------------------------------------------------
data PacketType = Connect | Disconnect | Event | Ack | Error | BinaryEvent | BinaryAck
  deriving (Bounded, Enum, Eq, Read, Show)


--------------------------------------------------------------------------------
parsePacketType :: Attoparsec.Parser PacketType
parsePacketType = asum
  [ Connect <$ AttoparsecC8.char '0'
  , Disconnect <$ AttoparsecC8.char '1'
  , Event <$ AttoparsecC8.char '2'
  , Ack <$ AttoparsecC8.char '3'
  , Error <$ AttoparsecC8.char '4'
  , BinaryEvent <$ AttoparsecC8.char '5'
  , BinaryAck <$ AttoparsecC8.char '6'
  ]


--------------------------------------------------------------------------------
encodePacketType :: PacketType -> Builder.Builder
encodePacketType t =
  case t of
    Connect -> Builder.char8 '0'
    Disconnect -> Builder.char8 '1'
    Event -> Builder.char8 '2'
    Ack -> Builder.char8 '3'
    Error -> Builder.char8 '4'
    BinaryEvent -> Builder.char8 '5'
    BinaryAck -> Builder.char8 '6'


--------------------------------------------------------------------------------
type Attachments = Int
type Namespace = Text.Text
type PacketId = Int

data Packet = Packet !PacketType !(Maybe Attachments) !Namespace !(Maybe PacketId) !(Maybe Aeson.Value)
  deriving (Eq, Show)


--------------------------------------------------------------------------------
parsePacket :: Attoparsec.Parser Packet
parsePacket = do
  t <- parsePacketType

  attachments <- if t `elem` [ BinaryEvent, BinaryAck ]
                   then (Just <$> numberStr) <* AttoparsecC8.char '-'
                   else pure Nothing

  namespace <- parseNamespace <|> pure "/"

  pIdStr <- (Just <$> numberStr) <|> pure Nothing

  Packet t attachments namespace pIdStr <$> ((Just <$> Aeson.json) <|> pure Nothing)

  where

  parseNamespace = do
    AttoparsecC8.peekChar' >>= guard . (== '/')
    Text.decodeUtf8 <$> AttoparsecC8.takeTill (== ',')

  numberStr = read <$> Attoparsec.many1 (AttoparsecC8.satisfy isDigit)


--------------------------------------------------------------------------------
encodePacket :: Packet -> Builder.Builder
encodePacket (Packet pt attachments n pId json) =
  encodePacketType pt <> fromMaybe mempty (Builder.lazyByteString . Aeson.encode <$> json)


--------------------------------------------------------------------------------
type EventHandler a = ReaderT Socket IO a


--------------------------------------------------------------------------------
initialize
  :: MonadIO m
  => EIO.ServerAPI m
  -> StateT RoutingTable m a
  -> IO (m ())
initialize api socketHandler = do
  eio <- EIO.initialize

  let
    eioHandler socket = do
      let wrappedSocket = Socket socket eio
      routingTable <- execStateT socketHandler (RoutingTable mempty (const (return ())))
      liftIO $ print (HashMap.keys $ rtEvents routingTable)

      return $ EIO.SocketApp
        { EIO.saApp = flip runReaderT wrappedSocket $ do
            emitPacketTo wrappedSocket (Packet Connect Nothing "/" Nothing Nothing)

            forever $ do
              EIO.TextPacket t <- liftIO (STM.atomically (EIO.receive socket))
              case Attoparsec.parseOnly parsePacket (Text.encodeUtf8 t) of
                Right (Packet Event _ _ _ (Just (Aeson.Array v))) | not (V.null v) -> do
                  case (V.unsafeHead v, V.unsafeTail v) of
                    (Aeson.String name, args) -> do
                      case name `HashMap.lookup` rtEvents routingTable of
                        Just handler -> void (runMaybeT (handler args))
                        Nothing -> return ()

                    other -> error $ "Unexpected arguments: " ++ show other

                Right e -> error $ "Unexpected parse: " ++ show e

                Left e -> error $ "Attoparsec failed: " ++ show e

        , EIO.saOnDisconnect = rtDisconnect routingTable (socketId wrappedSocket)
        }

  return (EIO.handler eio eioHandler api)


--------------------------------------------------------------------------------
data Socket = Socket { socketEIOSocket :: EIO.Socket
                     , socketEIO :: EIO.EngineIO
                     }

instance Eq Socket where
  (==) = (==) `Function.on` socketEIOSocket

instance Ord Socket where
  compare = comparing socketEIOSocket

socketId :: Socket -> EIO.SocketId
socketId = EIO.socketId . socketEIOSocket


--------------------------------------------------------------------------------
data RoutingTable = RoutingTable
  { rtEvents :: HashMap.HashMap Text.Text (Aeson.Array -> MaybeT (ReaderT Socket IO) ())
  , rtDisconnect :: EIO.SocketId -> IO ()
  }


--------------------------------------------------------------------------------
onJSON
  :: (MonadState RoutingTable m, Applicative m)
  => Text.Text
  -> (Aeson.Array -> EventHandler a)
  -> m ()
onJSON eventName handler =
  modify $ \rt -> rt
    { rtEvents =
        HashMap.insertWith (\new old json -> old json <|> new json)
                           eventName
                           (void . lift . handler)
                           (rtEvents rt)
    }


--------------------------------------------------------------------------------
on
  :: (MonadState RoutingTable m, Aeson.FromJSON arg, Applicative m)
  => Text.Text
  -> (arg -> EventHandler a)
  -> m ()
on eventName handler =
  let eventHandler v = do
        [x] <- return (V.toList v)
        case Aeson.fromJSON x of
          Aeson.Success s -> lift (handler s)
          Aeson.Error _ -> mzero

  in modify $ \rt -> rt
       { rtEvents =
           HashMap.insertWith (\new old json -> old json <|> new json)
                              eventName
                              (void . eventHandler)
                              (rtEvents rt)
       }


--------------------------------------------------------------------------------
on_
  :: (MonadState RoutingTable m, Applicative m)
  => Text.Text
  -> EventHandler a
  -> m ()
on_ eventName handler =
  let eventHandler v = guard (V.null v) >> lift handler

  in modify $ \rt -> rt
       { rtEvents =
           HashMap.insertWith (\new old json -> old json <|> new json)
                              eventName
                              (void . eventHandler)
                              (rtEvents rt)
       }


--------------------------------------------------------------------------------
appendDisconnectHandler
  :: MonadState RoutingTable m => (EIO.SocketId -> IO ()) -> m ()
appendDisconnectHandler handler = modify $ \rt -> rt
  { rtDisconnect = \sId -> do rtDisconnect rt sId
                              handler sId
  }

--------------------------------------------------------------------------------
emit :: (Aeson.ToJSON a, MonadReader Socket m, MonadIO m) => Text.Text -> a -> m ()
emit n x = emitJSON n (V.singleton (Aeson.toJSON x))


--------------------------------------------------------------------------------
emitTo :: (Aeson.ToJSON a, MonadIO m) => Socket -> Text.Text -> a -> m ()
emitTo s n x = emitJSONTo s n (V.singleton (Aeson.toJSON x))


--------------------------------------------------------------------------------
emitJSON :: (MonadReader Socket m, MonadIO m) => Text.Text -> Aeson.Array -> m ()
emitJSON n args = ask >>= \s -> emitJSONTo s n args


--------------------------------------------------------------------------------
emitJSONTo :: (MonadIO m) => Socket -> Text.Text -> Aeson.Array -> m ()
emitJSONTo s n args =
  emitPacketTo s (Packet Event Nothing "/" Nothing (Just (Aeson.Array (V.cons (Aeson.String n) args))))


--------------------------------------------------------------------------------
emitPacketTo :: (MonadIO m) => Socket -> Packet -> m ()
emitPacketTo socket packet =
  let bytes = LBS.toStrict (Builder.toLazyByteString (encodePacket packet))
  in liftIO (STM.atomically (EIO.send (socketEIOSocket socket) (EIO.TextPacket (Text.decodeUtf8 bytes))))


--------------------------------------------------------------------------------
broadcastJSON :: (MonadReader Socket m, MonadIO m) => Text.Text -> Aeson.Array -> m ()
broadcastJSON n args =
  broadcastPacket (Packet Event Nothing "/" Nothing (Just (Aeson.Array (V.cons (Aeson.String n) args))))


--------------------------------------------------------------------------------
broadcast :: (Aeson.ToJSON a, MonadReader Socket m, MonadIO m) => Text.Text -> a -> m ()
broadcast n x = broadcastJSON n (V.singleton (Aeson.toJSON x))


--------------------------------------------------------------------------------
broadcastPacket :: (MonadReader Socket m, MonadIO m) => Packet -> m ()
broadcastPacket packet = do
  let bytes = LBS.toStrict (Builder.toLazyByteString (encodePacket packet))
      eioPacket = EIO.TextPacket (Text.decodeUtf8 bytes)

  eio <- asks socketEIO
  t <- asks socketEIOSocket
  liftIO $ STM.atomically $ do
    sockets <- HashMap.delete (EIO.socketId t) <$> EIO.getOpenSockets eio
    forM_ sockets (flip EIO.send eioPacket)
