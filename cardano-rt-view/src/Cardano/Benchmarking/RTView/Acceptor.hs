{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.Acceptor
    ( launchMetricsAcceptor
    ) where


import           Cardano.Prelude
import           Prelude
                   ( String )
import qualified Data.Text as T
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
import           Cardano.BM.Data.Configuration
                   ( RemoteAddr (..) )
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
  -- TODO: currently there's only one |TraceAcceptor| in the configuration.
  -- We should support N |TraceAcceptor|s to receive metrics from N cardano nodes.
  getAcceptAt config >>= \case
    Just (RemoteSocket host port) -> do
      (tr :: Trace IO Text, switchBoard) <- Setup.setupTrace_ config "cardano-rt-view"
      let accTr   = appendName "acceptor" tr
          finalTr = appendName (mkAcceptorId host port) accTr
      TraceAcceptor.plugin config finalTr switchBoard >>= loadPlugin switchBoard
      -- Now all |LogObject|s received by this |TraceAcceptor| will be send to 'finalTr'.
      forever $ threadDelay 1000000
    Just (RemotePipe _) ->
      Ex.die $ "Only trace acceptor based on RemoteSocket is supported in config: " <> pathToConfig
    Nothing ->
      Ex.die $ "Trace acceptor isn't enabled in config: " <> pathToConfig

readConfig :: FilePath -> IO Configuration
readConfig pathToConfig = setup pathToConfig `catch` exceptHandler
 where
  exceptHandler :: IOException -> IO Configuration
  exceptHandler e = do
    putStrLn $ "Exception while reading configuration from: " <> pathToConfig
    throwIO e

-- | We want to identify each |TraceAcceptor| based on its socket.
mkAcceptorId :: String -> String -> Text
mkAcceptorId host port = anId
 where
  anId = T.replace "." "_" raw -- "127.0.0.1" -> "127_0_0_1"
  raw  = T.pack $ host <> "_" <> port
