{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.Acceptor
    ( launchMetricsAcceptor
    ) where


import           Cardano.Prelude
import           Control.Concurrent
                   ( threadDelay )
import           Control.Monad
                   ( forever )
import qualified System.Exit as Ex

import qualified Cardano.BM.Backend.TraceAcceptor as TraceAcceptor
import           Cardano.BM.Configuration
                   ( Configuration
                   , getAcceptAt, setup
                   )
import           Cardano.BM.Plugin
                   ( loadPlugin )
import qualified Cardano.BM.Setup as Setup
import           Cardano.BM.Trace
                   ( Trace )
import           Cardano.BM.Tracing
                   ( appendName )

import           Cardano.Benchmarking.RTView.CLI
                   ( RTViewParams (..) )

-- | It is assumed that there's at least one cardano-node process
--   that sends its metrics as |LogObject|s via |TraceForwarder|.
--   These |LogObject|s will be accepted and redirected to the
--   corresponding tracer.
launchMetricsAcceptor :: RTViewParams -> IO ()
launchMetricsAcceptor (RTViewParams pathToConfig) = do
  config <- readConfig pathToConfig
  getAcceptAt config >>= \case
    Just _ -> do
      (tr :: Trace IO Text, switchBoard) <- Setup.setupTrace_ config "cardano-rt-view"
      let accTr = appendName "acceptor" tr
      TraceAcceptor.plugin config accTr switchBoard >>= loadPlugin switchBoard
      -- Now |LogObject|s received by particular |TraceAcceptor| will be send to the tracer
      -- based on 'accTr' with corresponding 'nodeName' taken from configuration.
      forever $ threadDelay 1000000
    Nothing ->
      Ex.die $ "Trace acceptor isn't enabled in config: " <> pathToConfig

readConfig :: FilePath -> IO Configuration
readConfig pathToConfig = setup pathToConfig `catch` exceptHandler
 where
  exceptHandler :: IOException -> IO Configuration
  exceptHandler e = do
    putStrLn $ "Exception while reading configuration from: " <> pathToConfig
    throwIO e
