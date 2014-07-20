{-# LANGUAGE OverloadedStrings #-}
module Network.EngineIO.Snap (snapAPI) where

import Control.Applicative

import qualified Data.Attoparsec.Enumerator as Attoparsec
import qualified Data.ByteString.Builder as Builder
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Lazy as LMap
import qualified Network.EngineIO as EIO
import qualified Network.WebSockets.Snap as Snap
import qualified Snap.Core as Snap

--------------------------------------------------------------------------------
-- | A drop in 'EIO.ServerAPI' that works in any Snap monad - including both
-- @Handler@ and @Snap@.
snapAPI :: Snap.MonadSnap m => EIO.ServerAPI m
snapAPI = EIO.ServerAPI
  { EIO.srvSendResponse = \code ct body -> do
      Snap.modifyResponse $ Snap.setResponseCode code . Snap.setContentType ct
      Snap.writeLBS $ Builder.toLazyByteString body
      res <- Snap.getResponse
      Snap.finishWith res

  , EIO.srvGetQueryParams =
      LMap.foldlWithKey' (\m k v -> HashMap.insert k v m) HashMap.empty
        <$> Snap.getQueryParams

  , EIO.srvParseRequestBody = Snap.runRequestBody . Attoparsec.iterParser

  , EIO.srvGetRequestMethod = do
      m <- Snap.getsRequest Snap.rqMethod
      return $ case m of
        Snap.GET -> "GET"
        Snap.POST -> "POST"

  , EIO.srvRunWebSocket = Snap.runWebSocketsSnap

  }
