{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import qualified Data.ByteString as BS
import qualified Control.Concurrent.STM as STM
import qualified Network.EngineIO as EIO
import qualified Network.EngineIO.Snap as EIOSnap
import qualified Snap.CORS as CORS
import qualified Snap.Http.Server as Snap

--------------------------------------------------------------------------------
handleSocket socket = return EIO.SocketApp
  { EIO.saApp = do
      bytes <- BS.readFile "doge.png"
      STM.atomically $
        EIO.send socket (EIO.BinaryPacket bytes)

  , EIO.saOnDisconnect = return ()
  }


--------------------------------------------------------------------------------
main :: IO ()
main = do
  eio <- EIO.initialize
  Snap.quickHttpServe $ CORS.applyCORS CORS.defaultOptions $
    EIO.handler eio handleSocket EIOSnap.snapAPI
