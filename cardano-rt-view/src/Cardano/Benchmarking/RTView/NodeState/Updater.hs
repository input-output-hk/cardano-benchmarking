{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.NodeState.Updater
    ( launchNodeStateUpdater
    ) where

import           Cardano.Prelude

import           Data.List
                   ( (!!) )
import qualified Data.Map.Strict as Map
import           Data.Map.Strict
                   ( (!?) )
import qualified Data.Text as T

import           Control.Concurrent.MVar
                   ( modifyMVar_ )

import           Cardano.BM.Backend.Switchboard
                   ( Switchboard, readLogBuffer )
import           Cardano.BM.Data.Aggregated
                   ( Measurable (..) )
import           Cardano.BM.Data.LogItem
                   ( LOContent (..), LogObject (..) )
import           Cardano.BM.Trace
                   ( Trace
                   , logDebug
                   )

import           Cardano.Benchmarking.RTView.NodeState.Types
                   ( NodesState, NodeState (..), NodeInfo (..)
                   , NodeMetrics (..)
                   )

-- | This function is running in a separate thread.
--   It takes |LogObject|s with nodes' metrics from |LogBuffer|,
--   extracts these metrics and updates corresponding values
--   in the |NodesState|.
launchNodeStateUpdater
  :: Trace IO Text
  -> Switchboard a
  -> MVar NodesState
  -> IO ()
launchNodeStateUpdater tr switchBoard nsMVar = forever $ do
  logDebug tr "Try to update nodes' state..."
  -- Take current |LogObject|s from the |LogBuffer|.
  currentLogObjects <- readLogBuffer switchBoard
  forM_ currentLogObjects $ \(loggerName, logObject) -> do
    -- logDebug tr $ "UPDATE STATE, loggerName: " <> loggerName <> ", logObject: " <> (T.pack $ show logObject) 
    updateNodesState nsMVar loggerName logObject
  -- Check for updates in the |LogBuffer| every second.
  threadDelay 1000000

-- | Update NodeState for particular node based on loggerName.
updateNodesState
  :: MVar NodesState
  -> Text
  -> LogObject a
  -> IO ()
updateNodesState nsMVar loggerName (LogObject aName _ aContent) = do
  -- Check the name of the node this logObject came from.
  -- It is assumed that configuration contains correct names of remote nodes and
  -- loggers for them, for example:
  --   1. "a" - name of remote node in getAcceptAt.
  --   2. "cardano-rt-view.acceptor.a" - name of the logger which receives
  --        |LogObject|s from that node.
  -- So currently logger name for metrics has the following format:
  -- #buffered.cardano-rt-view.acceptor.a.NAME_OF_METRICS_GROUP.NAME_OF_METRIC,
  -- where "a" is the node name (from TraceAcceptor).
  let loggerNameParts = filter (not . T.null) $ T.splitOn "." loggerName
      nameOfNode = loggerNameParts !! 3

  modifyMVar_ nsMVar $ \currentNodesState -> do
    let nodesStateWith :: NodeState -> IO NodesState
        nodesStateWith newState = return $ Map.adjust (\_ -> newState) nameOfNode currentNodesState

    case currentNodesState !? nameOfNode of
      Just ns ->
        if "cardano.node.metrics" `T.isInfixOf` aName
          then
            case aContent of
              LogValue "Mem.resident" (PureI pages) ->
                nodesStateWith $ updateMemoryUsage ns pages
              LogValue "IO.rchar" (Bytes bytesWereRead) ->
                nodesStateWith $ updateDiskRead ns bytesWereRead
              LogValue "IO.wchar" (Bytes bytesWereWritten) ->
                nodesStateWith $ updateDiskWrite ns bytesWereWritten
              LogValue "Stat.utime" (PureI ticks) ->
                nodesStateWith $ updateCPUUsage ns ticks
              LogValue "Net.IpExt:InOctets" (Bytes inBytes) ->
                nodesStateWith $ updateNetworkIn ns inBytes
              LogValue "Net.IpExt:OutOctets" (Bytes outBytes) ->
                nodesStateWith $ updateNetworkOut ns outBytes
              LogValue "txsInMempool" (PureI txsInMempool) ->
                nodesStateWith $ updateMempoolTxs ns txsInMempool
              LogValue "mempoolBytes" (PureI mempoolBytes) ->
                nodesStateWith $ updateMempoolBytes ns mempoolBytes
              LogValue "txsProcessed" (PureI txsProcessed) ->
                nodesStateWith $ updateTxsProcessed ns txsProcessed
              _ -> return currentNodesState
          else
            case aContent of
              LogValue "density" (PureD density) ->
                nodesStateWith $ updateChainDensity ns density
              LogValue "connectedPeers" (PureI peersNum) ->
                nodesStateWith $ updatePeersNumber ns peersNum
              LogValue "blockNum" (PureI blockNum) ->
                nodesStateWith $ updateBlocksNumber ns blockNum
              LogValue "slotInEpoch" (PureI slotNum) ->
                nodesStateWith $ updateSlotInEpoch ns slotNum
              LogValue "epoch" (PureI epoch) ->
                nodesStateWith $ updateEpoch ns epoch
              _ -> return currentNodesState
      Nothing ->
        -- This is a problem, because it means that configuration is unexpected one:
        -- name of node in getAcceptAt doesn't correspond to the name of loggerName.
        return currentNodesState

-- Updaters for particular node state's fields.

updateMemoryUsage :: NodeState -> Integer -> NodeState
updateMemoryUsage ns pages = ns { nsMetrics = newNm }
 where
  newNm = currentNm
            { nmMemory    = mBytes
            , nmMemoryMax = maxMemory
            }
  mBytes    = fromIntegral (pages * pageSize) / 1024 / 1024 :: Double
  pageSize  = 4096 :: Integer
  maxMemory = max (nmMemoryMax currentNm) mBytes
  currentNm = nsMetrics ns

updateDiskRead :: NodeState -> Word64 -> NodeState
updateDiskRead ns _bytesWereRead = ns

updateDiskWrite :: NodeState -> Word64 -> NodeState
updateDiskWrite ns _bytesWereWritten = ns

updateCPUUsage :: NodeState -> Integer -> NodeState
updateCPUUsage ns _ticks = ns

updateNetworkIn :: NodeState -> Word64 -> NodeState
updateNetworkIn ns _inBytes = ns

updateNetworkOut :: NodeState -> Word64 -> NodeState
updateNetworkOut ns _outBytes = ns

updateMempoolTxs :: NodeState -> Integer -> NodeState
updateMempoolTxs ns txsInMempool = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmMempoolTxsNumber  = fromIntegral txsInMempool
      , nmMempoolTxsPercent =   fromIntegral txsInMempool
                              / fromIntegral (nmMempoolCapacity currentNm)
                              * 100.0
      }
  currentNm = nsMetrics ns

updateMempoolBytes :: NodeState -> Integer -> NodeState
updateMempoolBytes ns mempoolBytes = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmMempoolBytes = fromIntegral mempoolBytes
      , nmMempoolBytesPercent =   fromIntegral mempoolBytes
                                / fromIntegral (nmMempoolCapacityBytes currentNm)
                                * 100.0
      }
  currentNm = nsMetrics ns

updateTxsProcessed :: NodeState -> Integer -> NodeState
updateTxsProcessed ns txsProcessed = ns { nsInfo = newNi }
 where
  newNi = currentNi { niTxsProcessed = niTxsProcessed currentNi + fromIntegral txsProcessed }
  currentNi = nsInfo ns

updateChainDensity :: NodeState -> Double -> NodeState
updateChainDensity ns density = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niChainDensity = chainDensity }
  chainDensity = 0.05 + density * 100.0

updatePeersNumber :: NodeState -> Integer -> NodeState
updatePeersNumber ns peersNum = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niPeersNumber = fromIntegral peersNum }

updateBlocksNumber :: NodeState -> Integer -> NodeState
updateBlocksNumber ns blockNum = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niBlocksNumber = fromIntegral blockNum }

updateSlotInEpoch :: NodeState -> Integer -> NodeState
updateSlotInEpoch ns slotNum = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niSlot = fromIntegral slotNum }

updateEpoch :: NodeState -> Integer -> NodeState
updateEpoch ns epoch = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niEpoch = fromIntegral epoch }
