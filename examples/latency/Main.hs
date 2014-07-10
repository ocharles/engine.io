{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forever)

import qualified Control.Concurrent.STM as STM
import qualified Network.EngineIO as EIO
import qualified Network.EngineIO.Snap as EIOSnap
import qualified Snap.CORS as CORS
import qualified Snap.Http.Server as Snap


--------------------------------------------------------------------------------
main :: IO ()
main = do
  eio <- EIO.initialize
  Snap.quickHttpServe $ CORS.applyCORS CORS.defaultOptions $
    EIO.handler eio (return $ \s -> forever $ STM.atomically $ EIO.receive s >>= EIO.send s) EIOSnap.snapAPI
