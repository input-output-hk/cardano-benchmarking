{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.Acceptor
    ( launchMetricsAcceptor
    , launchFakeAcceptor
    ) where


import           Cardano.Prelude
import           Control.Concurrent
                   ( threadDelay )
import           Control.Monad
                   ( forever )
import           Control.Concurrent.MVar
                   ( modifyMVar_
                   )
import           System.Random
                   ( randomRIO )

import           Cardano.BM.Backend.LogBuffer
                   ( LogBuffer )
import           Cardano.BM.Backend.Switchboard
                   ( Switchboard
                   )
import qualified Cardano.BM.Backend.TraceAcceptor as TraceAcceptor
import           Cardano.BM.Configuration
                   ( Configuration )
import           Cardano.BM.Plugin
                   ( loadPlugin )
import           Cardano.BM.Trace
                   ( Trace )

import           Cardano.Benchmarking.RTView.NodeState
                   ( NodeState (..), NodeInfo (..), NodeMetrics (..) )

-- | It is assumed that there's at least one cardano-node process
--   that sends its metrics as |LogObject|s via |TraceForwarder|.
--   These |LogObject|s will be accepted and redirected to the
--   corresponding tracer.
launchMetricsAcceptor
  :: Configuration
  -> Trace IO Text
  -> Switchboard Text
  -> IO ()
launchMetricsAcceptor config accTr switchBoard = do
  TraceAcceptor.plugin config accTr switchBoard >>= loadPlugin switchBoard
  -- Now |LogObject|s received by particular |TraceAcceptor| will be send to the tracer
  -- based on 'accTr' with corresponding 'nodeName' taken from configuration. All these
  -- |LogObject|s will be stored in |LogBuffer|.
  forever $ threadDelay 1000000

-- | Fake acceptor, just for demonstration of "live" GUI.
--   Real acceptor takes these values from real |LogObject|s
--   received by |TraceAcceptor|s and stored in |LogBuffer|.
launchFakeAcceptor
  :: LogBuffer a
  -> MVar NodeState
  -> IO ()
launchFakeAcceptor _logBuffer nsMVar = forever $ do
  -- Fake new values for node state elements.
  
  fniEpoch        :: Int    <- randomRIO (1,   10)
  fniSlot         :: Int    <- randomRIO (1,   10)
  fniBlocksNumber :: Int    <- randomRIO (1,   10)
  fniChainDensity :: Double <- randomRIO (1.0, 100.0)
  fniTxsProcessed :: Int    <- randomRIO (1,   10)
  fniPort         :: Int    <- randomRIO (1,   10)
  fniPeersNumber  :: Int    <- randomRIO (1,   10)

  let fnmMempoolKBMax  :: Double = 800
  fnmMempoolKBPercent  :: Double <- randomRIO (1.0, 100.0)
  let fnmMempoolTxsMax :: Int = 200
  fnmMempoolTxsPercent :: Double <- randomRIO (1.0, 100.0)
  let fnmMemoryMax     :: Double = 200
  fnmMemoryPercent     :: Double <- randomRIO (1.0, 100.0)
  let fnmCPUMax        :: Double = 100
  fnmCPUPercent        :: Double <- randomRIO (1.0, 49.0)
  fnmDiskReadMax       :: Double <- randomRIO (50.0, 100.0)
  fnmDiskReadPercent   :: Double <- randomRIO (1.0, 49.0)
  fnmDiskWriteMax      :: Double <- randomRIO (50.0, 100.0)
  fnmDiskWritePercent  :: Double <- randomRIO (1.0, 49.0)
  fnmNetworkInMax      :: Double <- randomRIO (50.0, 100.0)
  fnmNetworkInPercent  :: Double <- randomRIO (1.0, 49.0)
  fnmNetworkOutMax     :: Double <- randomRIO (50.0, 100.0)
  fnmNetworkOutPercent :: Double <- randomRIO (1.0, 49.0)

  modifyMVar_ nsMVar $ \nodeState -> do
    let currentInfo    = nsInfo nodeState
        currentMetrics = nsMetrics nodeState

        newInfo = currentInfo
          { niEpoch        = fniEpoch
          , niSlot         = fniSlot
          , niBlocksNumber = fniBlocksNumber
          , niChainDensity = fniChainDensity
          , niTxsProcessed = fniTxsProcessed
          , niPort         = fniPort
          , niPeersNumber  = fniPeersNumber
          }

        newMetrics = currentMetrics
          { nmMempoolKBMax      = fnmMempoolKBMax
          , nmMempoolKBPercent  = fnmMempoolKBPercent
          , nmMempoolTxsMax     = fnmMempoolTxsMax
          , nmMempoolTxsPercent = fnmMempoolTxsPercent
          , nmMemoryMax         = fnmMemoryMax
          , nmMemoryPercent     = fnmMemoryPercent
          , nmCPUMax            = fnmCPUMax
          , nmCPUPercent        = fnmCPUPercent
          , nmDiskReadMax       = fnmDiskReadMax
          , nmDiskReadPercent   = fnmDiskReadPercent
          , nmDiskWriteMax      = fnmDiskWriteMax
          , nmDiskWritePercent  = fnmDiskWritePercent
          , nmNetworkInMax      = fnmNetworkInMax
          , nmNetworkInPercent  = fnmNetworkInPercent
          , nmNetworkOutMax     = fnmNetworkOutMax
          , nmNetworkOutPercent = fnmNetworkOutPercent
          }

    return $ nodeState
      { nsInfo = newInfo
      , nsMetrics = newMetrics
      }

  threadDelay 1000000
