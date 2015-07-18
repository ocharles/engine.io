{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Network.EngineIO.Wai (
    WaiMonad,
    toWaiApplication,
    waiAPI
    ) where


import Network.Wai
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Arrow (second)
import Data.Maybe (maybeToList)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)
import Data.Attoparsec.ByteString (parseOnly)
import Network.HTTP.Types.Header (hContentType)


import Network.HTTP.Types.Status as ST
import Network.HTTP.Types.URI as URI
import qualified Data.ByteString as BS
import qualified Network.EngineIO as EIO
import qualified Data.HashMap.Strict as HashMap
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS


newtype WaiMonad a = WaiMonad {
    runWaiMonad :: ExceptT Response (ReaderT Request IO) a
    } deriving (Monad, Functor, Applicative, MonadReader Request, MonadError Response, MonadIO)


toWaiApplication :: WaiMonad a -> Application
toWaiApplication sHandler req respond = do
    socket <- runReaderT (runExceptT (runWaiMonad sHandler)) req
    case socket of
        Left response -> respond response
        Right _ -> respond $ responseLBS status200 [("Content-Type", "text/html")] $ encodeUtf8 $ fromStrict ""


--------------------------------------------------------------------------------
-- | A drop in 'EIO.ServerAPI' that works with Wai.
waiAPI :: EIO.ServerAPI WaiMonad
waiAPI = EIO.ServerAPI
    { EIO.srvTerminateWithResponse = \code ct builder -> do
        let status = filter ((==) code . ST.statusCode) [ST.status100..ST.status511]
        case status of
            [] -> error "not a valid status code"
            (st:_) -> throwError (responseBuilder st [(hContentType, ct)] builder)

    , EIO.srvGetQueryParams = fmap (queryToHashMap . WAI.queryString) ask

    , EIO.srvParseRequestBody = \p -> do
        req <- ask
        b <- liftIO $ WAI.lazyRequestBody req
        return (parseOnly p $ toStrict b)

    , EIO.srvGetRequestMethod = fmap (WAI.requestMethod) ask

    , EIO.srvRunWebSocket = \app -> do
        req <- ask
        maybe (return ()) throwError (WaiWS.websocketsApp WS.defaultConnectionOptions app req)
    }


queryToHashMap :: URI.Query -> HashMap.HashMap BS.ByteString [BS.ByteString]
queryToHashMap = HashMap.fromListWith (++) . map (second maybeToList)

