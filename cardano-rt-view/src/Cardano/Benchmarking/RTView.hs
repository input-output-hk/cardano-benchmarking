module Cardano.Benchmarking.RTView
    ( runCardanoRTView
    ) where

import           Cardano.Prelude

import           Control.Concurrent.Async
                   ( async, waitBoth )

import           Cardano.Benchmarking.RTView.CLI
                   ( RTViewParams (..) )
import           Cardano.Benchmarking.RTView.Acceptor
                   ( launchMetricsAcceptor )
import           Cardano.Benchmarking.RTView.Server
                   ( launchServer )

-- | We launch 2 threads:
--   1. acceptor (it receives metrics from cardano-node processes),
--   2. server (it serves requests from user's browser and returns these metrics).
runCardanoRTView :: RTViewParams -> IO ()
runCardanoRTView rtViewParams = do
  acceptorThr <- async $ launchMetricsAcceptor rtViewParams
  serverThr <- async $ launchServer
  void $ waitBoth acceptorThr serverThr
