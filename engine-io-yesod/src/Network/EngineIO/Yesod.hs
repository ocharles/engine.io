{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Network.EngineIO.Yesod (yesodAPI) where

import Control.Applicative
import Data.Maybe (maybeToList)
import Control.Arrow (second)
import Data.Text (pack)
import Data.Conduit (($$))
import Data.Conduit.Lift (runCatchC)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Monoid (mappend)
import Control.Monad (unless)

import qualified Data.ByteString.Builder as Builder
import qualified Network.EngineIO as EIO
import qualified Yesod.Core as YC
import qualified Data.HashMap.Strict as HashMap
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import Network.HTTP.Types.Status as St

--------------------------------------------------------------------------------
-- | A drop in 'EIO.ServerAPI' that works in Yesod's 'Handler' monad.
yesodAPI :: (YC.MonadHandler m, YC.MonadBaseControl IO m) => EIO.ServerAPI m
yesodAPI = EIO.ServerAPI
  { EIO.srvTerminateWithResponse = \code ct builder -> do
      let status = filter ((==) code . St.statusCode) [St.status100..St.status511]
      case status of
        [] -> error "not a valid status code"
        (st:_) -> YC.sendResponseStatus st $ YC.TypedContent ct $
                    YC.toContent $ Builder.toLazyByteString builder

  , EIO.srvGetQueryParams = HashMap.fromListWith (++) . map (second maybeToList)
                              . WAI.queryString <$> YC.waiRequest

  , EIO.srvParseRequestBody = \p ->
      fmap (either (Left . show) Right) $
        YC.rawRequestBody $$ runCatchC (sinkParser p)

  , EIO.srvGetRequestMethod = WAI.requestMethod <$> YC.waiRequest

  , EIO.srvRunWebSocket = \app -> do
      req <- YC.waiRequest
      unless (WaiWS.isWebSocketsReq req) $ YC.invalidArgs ["not a websocket request"]
      YC.sendRawResponseNoConduit $ \src sink ->
        YC.liftIO $ WaiWS.runWebSockets WS.defaultConnectionOptions
          (WaiWS.getRequestHead req) app src sink
  }
