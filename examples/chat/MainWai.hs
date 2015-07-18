{-# LANGUAGE OverloadedStrings #-}


module Main where


import Data.Monoid ((<>))
import Chat (server, ServerState (..))
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)


import qualified Network.EngineIO.Wai as EIOWai
import qualified Control.Concurrent.STM as STM
import qualified Network.SocketIO as SocketIO


port :: Int
port = 3000


app :: EIOWai.WaiMonad () -> Application
app sHandle req respond
    | (elem "main.js" $ pathInfo req) = respond $ responseFile status200 [("Content-Type", "application/javascript")] "resources/main.js" Nothing
    | (elem "style.css" $ pathInfo req) = respond $ responseFile status200 [("Content-Type", "text/css")] "resources/style.css" Nothing
    | (elem "socket.io" $ pathInfo req) = EIOWai.toWaiApplication sHandle req respond
    | otherwise = respond $ responseFile status200 [("Content-Type", "text/html")] "resources/index.html" Nothing


main :: IO ()
main = do
    state <- ServerState <$> STM.newTVarIO 0
    sHandle <- SocketIO.initialize EIOWai.waiAPI (server state)
    putStrLn $ "Running on " <> show port
    run port $ app sHandle
