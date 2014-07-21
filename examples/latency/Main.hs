{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)

import qualified Control.Concurrent.STM as STM
import qualified Network.EngineIO as EIO
import qualified Network.EngineIO.Snap as EIOSnap
import qualified Snap.Core as Snap
import qualified Snap.Util.FileServe as Snap
import qualified Snap.Http.Server as Snap

import Paths_binary

--------------------------------------------------------------------------------
handler :: Monad m => EIO.Socket -> m EIO.SocketApp
handler s = return EIO.SocketApp
  { EIO.saApp = forever $
      STM.atomically $ EIO.receive s >>= EIO.send s

  , EIO.saOnDisconnect = return ()
  }


--------------------------------------------------------------------------------
main :: IO ()
main = do
  eio <- EIO.initialize
  Snap.quickHttpServe $ do
    dataDir <- liftIO getDataDir
    Snap.route [ ("/engine.io", EIO.handler eio handler EIOSnap.snapAPI)
               , ("/", Snap.serveDirectory dataDir)
               ]
