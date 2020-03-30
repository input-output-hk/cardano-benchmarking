{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.NodeState.Updater
    ( launchNodeStateUpdater
    ) where

import           Cardano.Prelude

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import           Data.Map.Strict
                   ( (!?) )
import qualified Data.Text as T
--import           Data.Time.Clock
--                   ( UTCTime (..) )

import           Control.Concurrent.MVar
                   ( modifyMVar_
                   )
import           System.Random
                   ( randomRIO )

import           Cardano.BM.Backend.Switchboard
                   ( Switchboard, readLogBuffer )
-- import           Cardano.BM.Configuration
--                    ( Configuration )

import           Cardano.Benchmarking.RTView.NodeState.Types
                   ( NodesState, NodeState (..), NodeInfo (..)
                   , NodeMetrics (..)
                   )

-- | This function is running in a separate thread.
--   It takes |LogObject|s with nodes' metrics from |LogBuffer|,
--   extracts these metrics and updates corresponding values
--   in the |NodesState|.
launchNodeStateUpdater
  :: Switchboard a
  -> MVar NodesState
  -> IO ()
launchNodeStateUpdater switchBoard nsMVar = forever $ do
  -- Take current |LogObject|s from the |LogBuffer|.
  currentLogObjects <- readLogBuffer switchBoard
  forM_ currentLogObjects $ \(loggerName, _logObject) -> do
    -- Check the name of node this logObject came from.
    -- It is assumed that configuration contains correct names of remote nodes and
    -- loggers for them, for example:
    --   1. "a" - name of remote node in getAcceptAt.
    --   2. "cardano-rt-view.acceptor.a" - name of the logger which receives
    --        |LogObject|s from that node.
    let nameOfNode = L.last . filter (not . T.null) $ T.splitOn "." loggerName
    modifyMVar_ nsMVar $ \currentNodesState -> do
      case currentNodesState !? nameOfNode of
        Nothing ->
          -- It's a problem, because it means that configuration is unexpected one:
          -- name of node in getAcceptAt doesn't correspond to the name of loggerName.
          return currentNodesState
        Just stateOfNode -> do
          fniEpoch        :: Int    <- randomRIO (1,   10)
          fniSlot         :: Int    <- randomRIO (1,   10)
          fniBlocksNumber :: Int    <- randomRIO (1,   10)
          fniChainDensity :: Double <- randomRIO (1.0, 100.0)
          fniTxsProcessed :: Int    <- randomRIO (1,   10)

          let fnmMempoolKBMax  :: Double = 800
          fnmMempoolKBPercent  :: Double <- randomRIO (1.0, 100.0)
          let fnmMempoolTxsCapacity :: Int = 200
          fnmMempoolTxsNumber  :: Int <- randomRIO (1, 45)
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

          let currentInfo    = nsInfo stateOfNode
              currentMetrics = nsMetrics stateOfNode

              newInfo = currentInfo
                { niEpoch        = fniEpoch
                , niSlot         = fniSlot
                , niBlocksNumber = fniBlocksNumber
                , niChainDensity = fniChainDensity
                , niTxsProcessed = fniTxsProcessed
                }

              newMetrics = currentMetrics
                { nmMempoolKBMax       = fnmMempoolKBMax
                , nmMempoolKBPercent   = fnmMempoolKBPercent
                , nmMempoolTxsCapacity = fnmMempoolTxsCapacity
                , nmMempoolTxsNumber   = fnmMempoolTxsNumber
                , nmMempoolTxsPercent  = fnmMempoolTxsPercent
                , nmMemoryMax          = fnmMemoryMax
                , nmMemoryPercent      = fnmMemoryPercent
                , nmCPUMax             = fnmCPUMax
                , nmCPUPercent         = fnmCPUPercent
                , nmDiskReadMax        = fnmDiskReadMax
                , nmDiskReadPercent    = fnmDiskReadPercent
                , nmDiskWriteMax       = fnmDiskWriteMax
                , nmDiskWritePercent   = fnmDiskWritePercent
                , nmNetworkInMax       = fnmNetworkInMax
                , nmNetworkInPercent   = fnmNetworkInPercent
                , nmNetworkOutMax      = fnmNetworkOutMax
                , nmNetworkOutPercent  = fnmNetworkOutPercent
                }

          let newStateOfNode = stateOfNode
                                 { nsInfo    = newInfo
                                 , nsMetrics = newMetrics
                                 }
          let newNodesState = Map.adjust (\_ -> newStateOfNode) nameOfNode currentNodesState
          return newNodesState

  threadDelay 1000000
