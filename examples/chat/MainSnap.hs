{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Chat (server, ServerState (..))
import Control.Applicative

import qualified Network.EngineIO.Snap as EIOSnap
import qualified Control.Concurrent.STM as STM
import qualified Snap.Core as Snap
import qualified Snap.Util.FileServe as Snap
import qualified Snap.Http.Server as Snap
import qualified Network.SocketIO as SocketIO

import Paths_chat (getDataDir)

main :: IO ()
main = do
  state <- ServerState <$> STM.newTVarIO 0
  socketIoHandler <- SocketIO.initialize EIOSnap.snapAPI (server state :: SocketIO.SocketHandler Snap.Snap ())
  dataDir <- getDataDir
  Snap.quickHttpServe $
    Snap.route [ ("/socket.io", socketIoHandler)
               , ("/", Snap.serveDirectory dataDir)
               ]
