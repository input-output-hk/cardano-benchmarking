{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.NodeState.Updater
    ( launchNodeStateUpdater
    ) where

import           Cardano.Prelude
import           Prelude
                   ( String )

import qualified Data.Aeson as A
import           Data.Char
                   ( isDigit )
import qualified Data.HashMap.Strict as HM
import           Data.List
                   ( (!!), findIndex, find )
import qualified Data.Map.Strict as Map
import           Data.Map.Strict
                   ( (!?) )
import qualified Data.Text as T
import           Data.Time.Calendar
                   ( Day (..) )
import           Data.Time.Clock
                   ( UTCTime (..)
                   , addUTCTime, diffUTCTime, getCurrentTime
                   )

import           Control.Concurrent.MVar
                   ( modifyMVar_ )

import           Cardano.BM.Backend.Switchboard
                   ( Switchboard, readLogBuffer )
import           Cardano.BM.Data.Aggregated
                   ( Measurable (..) )
import           Cardano.BM.Data.LogItem
                   ( LOContent (..), LOMeta (..), LogObject (..)
                   , MonitorAction (..)
                   , utc2ns
                   )
import           Cardano.BM.Data.Severity
                   ( Severity (..) )
import           Cardano.BM.Trace
                   ( Trace
                   , logDebug
                   )

import           Cardano.Benchmarking.RTView.NodeState.Types
                   ( NodesState, NodeState (..), NodeInfo (..)
                   , NodeMetrics (..), NodeError (..), PeerInfo (..)
                   )

-- | This function is running in a separate thread.
--   It takes |LogObject|s with nodes' metrics from |LogBuffer|,
--   extracts these metrics and updates corresponding values
--   in the |NodesState|.
launchNodeStateUpdater
  :: Trace IO Text
  -> Switchboard Text
  -> MVar NodesState
  -> IO ()
launchNodeStateUpdater tr switchBoard nsMVar = forever $ do
  logDebug tr "Try to update nodes' state..."
  -- Take current |LogObject|s from the |LogBuffer|.
  currentLogObjects <- readLogBuffer switchBoard
  forM_ currentLogObjects $ \(loggerName, logObject) ->
    updateNodesState nsMVar loggerName logObject
  -- Check for updates in the |LogBuffer| every second.
  threadDelay 1000000

-- | Update NodeState for particular node based on loggerName.
updateNodesState
  :: MVar NodesState
  -> Text
  -> LogObject Text
  -> IO ()
updateNodesState nsMVar loggerName (LogObject aName aMeta aContent) = do
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

  now <- liftIO getCurrentTime

  modifyMVar_ nsMVar $ \currentNodesState -> do
    let nodesStateWith :: NodeState -> IO NodesState
        nodesStateWith newState = return $ Map.adjust (\_ -> newState) nameOfNode currentNodesState

    case currentNodesState !? nameOfNode of
      Just ns' -> do
        -- Since the node sent this LogObject, we have to update the uptime of that node.
        let ns = updateNodeUpTime ns' now
        if | itIsErrorMessage aMeta ->
              nodesStateWith $ updateNodeErrors ns aMeta aContent
           | "cardano.node.metrics" `T.isInfixOf` aName ->
            case aContent of
              LogValue "Mem.resident" (PureI pages) ->
                nodesStateWith $ updateMemoryUsage ns pages
              LogValue "IO.rchar" (Bytes bytesWereRead) ->
                nodesStateWith $ updateDiskRead ns bytesWereRead aMeta
              LogValue "IO.wchar" (Bytes bytesWereWritten) ->
                nodesStateWith $ updateDiskWrite ns bytesWereWritten aMeta
              LogValue "Stat.utime" (PureI ticks) ->
                nodesStateWith $ updateCPUUsage ns ticks aMeta
              LogValue "Net.IpExt:InOctets" (Bytes inBytes) ->
                nodesStateWith $ updateNetworkIn ns inBytes aMeta
              LogValue "Net.IpExt:OutOctets" (Bytes outBytes) ->
                nodesStateWith $ updateNetworkOut ns outBytes aMeta
              LogValue "txsInMempool" (PureI txsInMempool) ->
                nodesStateWith $ updateMempoolTxs ns txsInMempool
              LogValue "mempoolBytes" (PureI mempoolBytes) ->
                nodesStateWith $ updateMempoolBytes ns mempoolBytes
              LogValue "txsProcessed" (PureI txsProcessed) ->
                nodesStateWith $ updateTxsProcessed ns txsProcessed
              _ -> return currentNodesState
           | "cardano.node-metrics" `T.isInfixOf` aName ->
              case aContent of
                LogValue "RTS.bytesAllocated" (Bytes bytesAllocated) ->
                  nodesStateWith $ updateBytesAllocated ns bytesAllocated
                LogValue "RTS.usedMemBytes" (Bytes usedMemBytes) ->
                  nodesStateWith $ updateBytesUsed ns usedMemBytes
                LogValue "RTS.gcCpuNs" (Nanoseconds gcCpuNs) ->
                  nodesStateWith $ updateGcCpuNs ns gcCpuNs
                LogValue "RTS.gcElapsedNs" (Nanoseconds gcElapsedNs) ->
                  nodesStateWith $ updateGcElapsedNs ns gcElapsedNs
                LogValue "RTS.gcNum" (PureI gcNum) ->
                  nodesStateWith $ updateGcNum ns gcNum
                LogValue "RTS.gcMajorNum" (PureI gcMajorNum) ->
                  nodesStateWith $ updateGcMajorNum ns gcMajorNum
                _ -> return currentNodesState
           | "cardano.node.BlockFetchDecision" `T.isInfixOf` aName ->
              case aContent of
                LogValue "connectedPeers" (PureI peersNum) ->
                  nodesStateWith $ updatePeersNumber ns peersNum
                LogStructured peerInfo ->
                  nodesStateWith $ updatePeerEndpoint ns peerInfo
                _ -> return currentNodesState
           | "cardano.node.ChainSyncProtocol" `T.isInfixOf` aName ->
              case aContent of
                LogMessage traceLabelPeer ->
                  nodesStateWith $ updatePeerInfo ns traceLabelPeer
                _ -> return currentNodesState
           | "cardano.node.release" `T.isInfixOf` aName ->
              case aContent of
                LogMessage release ->
                  nodesStateWith $ updateNodeRelease ns release
                _ -> return currentNodesState
           | "cardano.node.version" `T.isInfixOf` aName ->
              case aContent of
                LogMessage version ->
                  nodesStateWith $ updateNodeVersion ns version
                _ -> return currentNodesState
           | "cardano.node.commit" `T.isInfixOf` aName ->
              case aContent of
                LogMessage commit ->
                  nodesStateWith $ updateNodeCommit ns commit
                _ -> return currentNodesState
           | otherwise ->
              case aContent of
                LogValue "density" (PureD density) ->
                  nodesStateWith $ updateChainDensity ns density
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

-- | If this is an error message, it will be shown in "Errors" tab in GUI.
itIsErrorMessage :: LOMeta -> Bool
itIsErrorMessage aMeta =
  case severity aMeta of
    Warning   -> True
    Error     -> True
    Alert     -> True
    Emergency -> True
    _         -> False
    -- 'Critical' is skipped because many non-error metrics have this severity.

-- Updaters for particular node state's fields.

updateNodeUpTime :: NodeState -> UTCTime -> NodeState
updateNodeUpTime ns now = ns { nsInfo = newNi }
 where
  newNi =
    currentNi
      { niStartTime = startTime
      , niUpTime    = upTime
      }
  currentNi = nsInfo ns
  currentStartTime@(UTCTime day time) = niStartTime currentNi
  startTime =
    if day == (ModifiedJulianDay 0) && time == 0
      then now -- This is the first LogObject from the node, update startTime.
      else currentStartTime -- Node already sent something earlier, keep startTime.
  diffBetweenNowAndStart = diffUTCTime now startTime
  upTime = addUTCTime diffBetweenNowAndStart (UTCTime (ModifiedJulianDay 0) 0)

updateNodeErrors :: Show a => NodeState -> LOMeta -> LOContent a -> NodeState
updateNodeErrors ns (LOMeta timeStamp _ _ sev _) aContent = ns { nsInfo = newNi }
 where
  newNi = currentNi { niNodeErrors = currentErrors ++ newError }
  currentNi = nsInfo ns
  currentErrors = niNodeErrors currentNi
  newError =
    case errorMessage of
      "" -> []
      _  -> [NodeError timeStamp sev errorMessage]
  errorMessage =
    case aContent of
      LogMessage msg -> show msg
      LogError eMsg -> T.unpack eMsg
      LogValue msg measurable -> T.unpack msg <> ", " <> prepared measurable
      LogStructured obj -> show obj
      MonitoringEffect (MonitorAlert msg) -> "Monitor alert: " <> T.unpack msg
      MonitoringEffect _ -> ""
      _ -> ""
  prepared :: Measurable -> String
  prepared (Microseconds v) = show v <> " mcs"
  prepared (Nanoseconds v)  = show v <> " ns"
  prepared (Seconds v)      = show v <> " s"
  prepared (Bytes v)        = show v <> " bytes"
  prepared (PureD v)        = show v
  prepared (PureI v)        = show v
  prepared _                = ""

-- | Since we know the format of JSON-representation in log messages,
--   we extract values as a raw strings.
--     * Pros: the simplest solution without dependencies from ouroboros libraries.
--     * Cons: primitive text parsing.
--   At the worst, it can be treated as a temporary solution.
updatePeerEndpoint :: NodeState -> HM.HashMap Text A.Value -> NodeState
updatePeerEndpoint ns peerInfo = ns { nsInfo = newNi }
 where
  newNi = currentNi { niPeersInfo = newPeersInfo }
  currentNi = nsInfo ns
  currentPeers = niPeersInfo currentNi
  connectionInfo = case HM.lookup "peer" peerInfo of
                     Just (A.String info) -> info
                     _ -> ""
  endpoint = getValueOf "remoteAddress" endpointOnly $ T.words connectionInfo
  endpointIsHere = isJust $ find (\(PeerInfo ep _ _) -> ep == endpoint) currentPeers
  newPeersInfo = if endpointIsHere
                   then currentPeers
                   else currentPeers ++ [PeerInfo endpoint "-" "-"]
  -- TODO: this function does NOT remove peer's info if corresponding
  -- peer was disconnected. But we plan to add check of outdated metrics,
  -- so it will be implemented later.

updatePeerInfo :: NodeState -> Text -> NodeState
updatePeerInfo ns peerInfo = ns { nsInfo = newNi }
 where
  newNi = currentNi { niPeersInfo = newPeersInfo }
  currentNi = nsInfo ns
  currentPeers = niPeersInfo currentNi
  newPeersInfo = map (\pI@(PeerInfo ep _ _) ->
                       if ep == endpoint && slotAndPortAreNew
                         then PeerInfo ep slotNo blockNo
                         else pI)
                     currentPeers
  endpoint = getValueOf "remoteAddress" endpointOnly $ T.words info
  slotNo   = getValueOf "unSlotNo" numbersOnly tipInfo'
  blockNo  = getValueOf "unBlockNo" numbersOnly tipInfo'
  slotAndPortAreNew = (not . null $ slotNo) && (not . null $ blockNo)
  (info, tipInfo) = T.breakOn "Tip" peerInfo
  tipInfo' = T.words tipInfo

getValueOf
  :: Text
  -> (Text -> Text)
  -> [Text]
  -> String
getValueOf elemName aFilter parts = 
  case findIndex (\n -> elemName `T.isInfixOf` n) parts of
    Just i  -> T.unpack . aFilter $ parts !! (i + 2) -- Skip '=' mark.
    Nothing -> ""

endpointOnly :: Text -> Text
endpointOnly = T.filter (\c -> isDigit c || c == '.' || c == ':')

numbersOnly :: Text -> Text
numbersOnly = T.filter isDigit

---

updateNodeRelease :: NodeState -> Text -> NodeState
updateNodeRelease ns release = ns { nsInfo = newNi }
 where
  newNi = currentNi { niNodeRelease = T.unpack release }
  currentNi = nsInfo ns

updateNodeVersion :: NodeState -> Text -> NodeState
updateNodeVersion ns version = ns { nsInfo = newNi }
 where
  newNi = currentNi { niNodeVersion = T.unpack version }
  currentNi = nsInfo ns

updateNodeCommit :: NodeState -> Text -> NodeState
updateNodeCommit ns commit = ns { nsInfo = newNi }
 where
  newNi =
    currentNi
      { niNodeCommit      = T.unpack commit
      , niNodeShortCommit = take 7 $ T.unpack commit
      }
  currentNi = nsInfo ns

updateMemoryUsage :: NodeState -> Integer -> NodeState
updateMemoryUsage ns pages = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmMemory         = mBytes
      , nmMemoryMax      = newMax
      , nmMemoryMaxTotal = newMaxTotal
      , nmMemoryPercent  = mBytes / newMaxTotal * 100.0
      }
  currentNm   = nsMetrics ns
  prevMax     = nmMemoryMax currentNm
  newMax      = max prevMax mBytes
  newMaxTotal = max newMax 200.0
  mBytes      = fromIntegral (pages * pageSize) / 1024 / 1024 :: Double
  pageSize    = 4096 :: Integer

updateDiskRead :: NodeState -> Word64 -> LOMeta -> NodeState
updateDiskRead ns bytesWereRead meta = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmDiskUsageR         = currentDiskRate
      , nmDiskUsageRPercent  = diskUsageRPercent
      , nmDiskUsageRLast     = bytesWereRead
      , nmDiskUsageRNs       = currentTimeInNs
      , nmDiskUsageRMax      = maxDiskRate
      , nmDiskUsageRMaxTotal = max maxDiskRate 1.0
      }
  currentNm         = nsMetrics ns
  currentTimeInNs   = utc2ns (tstamp meta)
  timeDiff          = fromIntegral (currentTimeInNs - nmDiskUsageRNs currentNm) :: Double
  timeDiffInSecs    = timeDiff / 1000000000
  bytesDiff         = fromIntegral (bytesWereRead - nmDiskUsageRLast currentNm) :: Double
  bytesDiffInKB'    = bytesDiff / 1024
  bytesDiffInKB     = if bytesDiffInKB' > 500.0 -- To prevent an overflow if the node was restarted.
                        then 1.0
                        else bytesDiffInKB'
  currentDiskRate   = bytesDiffInKB / timeDiffInSecs
  maxDiskRate       = max currentDiskRate $ nmDiskUsageRMax currentNm
  diskUsageRPercent = currentDiskRate / (maxDiskRate / 100.0)

updateDiskWrite :: NodeState -> Word64 -> LOMeta -> NodeState
updateDiskWrite ns bytesWereWritten meta = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmDiskUsageW         = currentDiskRate
      , nmDiskUsageWPercent  = diskUsageWPercent
      , nmDiskUsageWLast     = bytesWereWritten
      , nmDiskUsageWNs       = currentTimeInNs
      , nmDiskUsageWMax      = maxDiskRate
      , nmDiskUsageWMaxTotal = max maxDiskRate 1.0
      }
  currentNm         = nsMetrics ns
  currentTimeInNs   = utc2ns (tstamp meta)
  timeDiff          = fromIntegral (currentTimeInNs - nmDiskUsageWNs currentNm) :: Double
  timeDiffInSecs    = timeDiff / 1000000000
  bytesDiff         = fromIntegral (bytesWereWritten - nmDiskUsageWLast currentNm) :: Double
  bytesDiffInKB'    = bytesDiff / 1024
  bytesDiffInKB     = if bytesDiffInKB' > 500.0 -- To prevent an overflow if the node was restarted.
                        then 1.0
                        else bytesDiffInKB'
  currentDiskRate   = bytesDiffInKB / timeDiffInSecs
  maxDiskRate       = max currentDiskRate $ nmDiskUsageWMax currentNm
  diskUsageWPercent = currentDiskRate / (maxDiskRate / 100.0)

updateCPUUsage :: NodeState -> Integer -> LOMeta -> NodeState
updateCPUUsage ns ticks meta = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmCPUPercent = cpuperc * 100.0
      , nmCPULast    = ticks
      , nmCPUNs      = tns
      }
  currentNm = nsMetrics ns
  tns       = utc2ns $ tstamp meta
  tdiff     = min 1 $ (fromIntegral (tns - nmCPUNs currentNm)) / 1000000000 :: Double
  cpuperc   = (fromIntegral (ticks - nmCPULast currentNm)) / (fromIntegral clktck) / tdiff
  clktck    = 100 :: Integer

updateNetworkIn :: NodeState -> Word64 -> LOMeta -> NodeState
updateNetworkIn ns inBytes meta = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmNetworkUsageIn         = currentNetRate
      , nmNetworkUsageInPercent  = currentNetRate / (maxNetRate / 100.0)
      , nmNetworkUsageInLast     = inBytes
      , nmNetworkUsageInNs       = currentTimeInNs
      , nmNetworkUsageInMax      = maxNetRate
      , nmNetworkUsageInMaxTotal = max maxNetRate 1.0
      }
  currentNm       = nsMetrics ns
  currentTimeInNs = utc2ns (tstamp meta)
  timeDiff        = fromIntegral (currentTimeInNs - nmNetworkUsageInNs currentNm) :: Double
  timeDiffInSecs  = timeDiff / 1000000000
  bytesDiff       = fromIntegral (inBytes - nmNetworkUsageInLast currentNm) :: Double
  bytesDiffInKB   = bytesDiff / 1024
  currentNetRate  = bytesDiffInKB / timeDiffInSecs
  maxNetRate      = max currentNetRate $ nmNetworkUsageInMax currentNm

updateNetworkOut :: NodeState -> Word64 -> LOMeta -> NodeState
updateNetworkOut ns outBytes meta = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmNetworkUsageOut         = currentNetRate
      , nmNetworkUsageOutPercent  = currentNetRate / (maxNetRate / 100.0)
      , nmNetworkUsageOutLast     = outBytes
      , nmNetworkUsageOutNs       = currentTimeInNs
      , nmNetworkUsageOutMax      = maxNetRate
      , nmNetworkUsageOutMaxTotal = max maxNetRate 1.0
      }
  currentNm       = nsMetrics ns
  currentTimeInNs = utc2ns (tstamp meta)
  timeDiff        = fromIntegral (currentTimeInNs - nmNetworkUsageOutNs currentNm) :: Double
  timeDiffInSecs  = timeDiff / 1000000000
  bytesDiff       = fromIntegral (outBytes - nmNetworkUsageOutLast currentNm) :: Double
  bytesDiffInKB   = bytesDiff / 1024
  currentNetRate  = bytesDiffInKB / timeDiffInSecs
  maxNetRate      = max currentNetRate $ nmNetworkUsageOutMax currentNm

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
  newNi = currentNi { niTxsProcessed = niTxsProcessed currentNi + txsProcessed }
  currentNi = nsInfo ns

updateBytesAllocated :: NodeState -> Word64 -> NodeState
updateBytesAllocated ns bytesAllocated = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmRTSMemoryAllocated = mBytes
      }
  currentNm = nsMetrics ns
  mBytes    = fromIntegral (bytesAllocated) / 1024 / 1024 :: Double

updateBytesUsed :: NodeState -> Word64 -> NodeState
updateBytesUsed ns usedMemBytes = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmRTSMemoryUsed = mBytes
      , nmRTSMemoryUsedPercent =   mBytes
                                 / (nmRTSMemoryAllocated currentNm)
                                 * 100.0
      }
  currentNm = nsMetrics ns
  mBytes    = fromIntegral (usedMemBytes) / 1024 / 1024 :: Double

updateGcCpuNs :: NodeState -> Word64 -> NodeState
updateGcCpuNs ns gcCpuNs = ns { nsMetrics = newNm }
 where
  newNm     = currentNm { nmRTSGcCpu = seconds }
  currentNm = nsMetrics ns
  seconds   = (fromIntegral gcCpuNs) / 1000000000 :: Double

updateGcElapsedNs :: NodeState -> Word64 -> NodeState
updateGcElapsedNs ns gcElapsedNs = ns { nsMetrics = newNm }
 where
  newNm     = currentNm { nmRTSGcElapsed = seconds }
  currentNm = nsMetrics ns
  seconds   = (fromIntegral gcElapsedNs) / 1000000000 :: Double

updateGcNum :: NodeState -> Integer -> NodeState
updateGcNum ns gcNum = ns { nsMetrics = newNm }
 where
  newNm     = currentNm { nmRTSGcNum = gcNum }
  currentNm = nsMetrics ns

updateGcMajorNum :: NodeState -> Integer -> NodeState
updateGcMajorNum ns gcMajorNum = ns { nsMetrics = newNm }
 where
  newNm     = currentNm { nmRTSGcMajorNum = gcMajorNum }
  currentNm = nsMetrics ns

updateChainDensity :: NodeState -> Double -> NodeState
updateChainDensity ns density = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niChainDensity = chainDensity }
  chainDensity = 0.05 + density * 100.0

updatePeersNumber :: NodeState -> Integer -> NodeState
updatePeersNumber ns peersNum = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niPeersNumber = peersNum }

updateBlocksNumber :: NodeState -> Integer -> NodeState
updateBlocksNumber ns blockNum = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niBlocksNumber = blockNum }

updateSlotInEpoch :: NodeState -> Integer -> NodeState
updateSlotInEpoch ns slotNum = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niSlot = slotNum }

updateEpoch :: NodeState -> Integer -> NodeState
updateEpoch ns epoch = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niEpoch = epoch }
