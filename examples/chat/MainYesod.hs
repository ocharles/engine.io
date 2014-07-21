{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Main where

import Control.Applicative
import Chat (server, ServerState (..))
import qualified Yesod.Core as YC
import qualified Control.Concurrent.STM as STM
import qualified Network.SocketIO as SocketIO
import qualified Network.EngineIO.Yesod as EIOYesod

import Paths_chat (getDataDir)

data YesodChat = YesodChat { socketIoHandler :: YC.HandlerT YesodChat IO () }

YC.mkYesod "YesodChat" [YC.parseRoutesNoCheck|
/ IndexR GET
/main.js MainJSR GET
/style.css StyleCSSR GET
/socket.io/ SocketIOR
|]

instance YC.Yesod YesodChat where
  -- do not redirect /socket.io/?bla=blub to /socket.io?bla=blub
  cleanPath _ ["socket.io",""] = Right ["socket.io"]
  cleanPath _ p = Right p

getIndexR :: Handler ()
getIndexR = do
  dataDir <- YC.liftIO getDataDir
  YC.sendFile "text/html" $ dataDir ++ "/index.html"

getStyleCSSR :: Handler ()
getStyleCSSR = do
  dataDir <- YC.liftIO getDataDir
  YC.sendFile "text/css" $ dataDir ++ "/style.css"

getMainJSR :: Handler ()
getMainJSR = do
  dataDir <- YC.liftIO getDataDir
  YC.sendFile "application/javascript" $ dataDir ++ "/main.js"

handleSocketIOR :: Handler ()
handleSocketIOR = YC.getYesod >>= socketIoHandler

main :: IO ()
main = do
  state <- ServerState <$> STM.newTVarIO 0
  app <- YesodChat <$> SocketIO.initialize EIOYesod.yesodAPI (server state)
  YC.warp 8000 app
