{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.Acceptor
    ( launchMetricsAcceptor
    ) where

import           Cardano.Prelude

import           Cardano.BM.Backend.Switchboard
                   ( Switchboard )
import qualified Cardano.BM.Backend.TraceAcceptor as TraceAcceptor
import           Cardano.BM.Configuration
                   ( Configuration )
import           Cardano.BM.IOManager
import           Cardano.BM.Plugin
                   ( loadPlugin )
import           Cardano.BM.Trace
                   ( Trace )

-- | It is assumed that there's at least one cardano-node process
--   that sends its metrics as |LogObject|s via |TraceForwarder|.
--   These |LogObject|s will be accepted by |TraceAcceptor|s and
--   redirected to the corresponding tracers and finally stored
--   in the |LogBuffer|.
launchMetricsAcceptor
  :: Configuration
  -> Trace IO Text
  -> Switchboard Text
  -> IO ()
launchMetricsAcceptor config accTr switchBoard =
  withIOManager $ \iomgr -> do
    TraceAcceptor.plugin iomgr config accTr switchBoard >>= loadPlugin switchBoard
    forever $ threadDelay 1000000
