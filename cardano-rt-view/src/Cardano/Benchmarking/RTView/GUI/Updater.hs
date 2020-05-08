{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.GUI.Updater
    ( updateGUI
    ) where

import           Cardano.Prelude hiding ( (%) )
import           Prelude
                   ( String )
import qualified Data.List as L
import           Data.Map.Strict
                   ( (!) )
import           Data.Time.Calendar
                   ( Day (..) )
import           Data.Time.Clock
                   ( NominalDiffTime, UTCTime (..)
                   , addUTCTime
                   )
import           Data.Time.Format
                   ( defaultTimeLocale, formatTime )
import           Data.Text
                   ( unpack )
import           Formatting
                   ( sformat, fixed, (%) )
import           GHC.Clock
                   ( getMonotonicTimeNSec )
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
                   ( Element, UI
                   , (#), (#.), (#+), children, element
                   , set, style, text
                   )

import           Cardano.BM.Data.Configuration
                   ( RemoteAddrNamed (..), RemoteAddr (..) )
import           Cardano.BM.Data.Severity
                   ( Severity (..) )
import           Cardano.Benchmarking.RTView.CLI
                   ( RTViewParams (..) )
import           Cardano.Benchmarking.RTView.GUI.Elements
                   ( ElementName (..), ElementValue (..)
                   , NodeStateElements, NodesStateElements
                   )
import           Cardano.Benchmarking.RTView.NodeState.Types
                   ( NodeInfo (..), NodeMetrics (..), NodeError (..)
                   , NodeState (..), NodesState, PeerInfo (..)
                   )

-- | This function is calling by the timer. It updates the node' state elements
--   on the page automatically, because threepenny-gui is based on websockets.
updateGUI
  :: NodesState
  -> RTViewParams
  -> [RemoteAddrNamed]
  -> NodesStateElements
  -> UI ()
updateGUI nodesState params acceptors nodesStateElems =
  forM_ nodesStateElems $ \(nameOfNode, elements) -> do
    let nodeState = nodesState ! nameOfNode
        (acceptorHost, acceptorPort) = findTraceAcceptorNetInfo nameOfNode acceptors

    let ni = nsInfo nodeState
        nm = nsMetrics nodeState
        activeNodeMark = unpack nameOfNode

    void $ updateElementValue (ElementString  $ niNodeRelease ni)             $ elements ! ElNodeRelease
    void $ updateElementValue (ElementString  $ niNodeVersion ni)             $ elements ! ElNodeVersion
    void $ updateElementValue (ElementString  $ niNodePlatform ni)            $ elements ! ElNodePlatform
    void $ updateNodeCommit   (niNodeCommit ni) (niNodeShortCommit ni)        $ elements ! ElNodeCommitHref
    void $ updateElementValue (ElementString activeNodeMark)                  $ elements ! ElActiveNode
    void $ updateNodeUpTime   (niUpTime ni)                                   $ elements ! ElUptime
    void $ updateElementValue (ElementInteger $ niEpoch ni)                   $ elements ! ElEpoch
    void $ updateElementValue (ElementInteger $ niSlot ni)                    $ elements ! ElSlot
    void $ updateElementValue (ElementInteger $ niBlocksNumber ni)            $ elements ! ElBlocksNumber
    void $ updateElementValue (ElementDouble  $ niChainDensity ni)            $ elements ! ElChainDensity
    void $ updateElementValue (ElementInteger $ niTxsProcessed ni)            $ elements ! ElTxsProcessed
    void $ updateElementValue (ElementInteger $ niPeersNumber ni)             $ elements ! ElPeersNumber
    void $ updatePeersList    (niPeersInfo ni)                                $ elements ! ElPeersList
    void $ updateElementValue (ElementString  acceptorHost)                   $ elements ! ElTraceAcceptorHost
    void $ updateElementValue (ElementString  acceptorPort)                   $ elements ! ElTraceAcceptorPort
    void $ updateErrorsList   (niNodeErrors ni)                               $ elements ! ElNodeErrors
    void $ updateElementValue (ElementWord64  $ nmMempoolTxsNumber nm)        $ elements ! ElMempoolTxsNumber
    void $ updateElementValue (ElementDouble  $ nmMempoolTxsPercent nm)       $ elements ! ElMempoolTxsPercent
    void $ updateElementValue (ElementWord64  $ nmMempoolBytes nm)            $ elements ! ElMempoolBytes
    void $ updateElementValue (ElementDouble  $ nmMempoolBytesPercent nm)     $ elements ! ElMempoolBytesPercent
    void $ updateElementValue (ElementWord64  $ nmMempoolCapacity nm)         $ elements ! ElMempoolCapacity
    void $ updateElementValue (ElementWord64  $ nmMempoolCapacityBytes nm)    $ elements ! ElMempoolCapacityBytes
    void $ updateElementValue (ElementDouble  $ nmMemory nm)                  $ elements ! ElMemory
    void $ updateElementValue (ElementDouble  $ nmMemoryMax nm)               $ elements ! ElMemoryMax
    void $ updateElementValue (ElementDouble  $ nmMemoryMaxTotal nm)          $ elements ! ElMemoryMaxTotal
    void $ updateElementValue (ElementDouble  $ nmMemoryPercent nm)           $ elements ! ElMemoryPercent
    void $ updateElementValue (ElementDouble  $ nmCPUPercent nm)              $ elements ! ElCPUPercent
    void $ updateElementValue (ElementDouble  $ nmDiskUsageR nm)              $ elements ! ElDiskUsageR
    void $ updateElementValue (ElementDouble  $ nmDiskUsageRMaxTotal nm)      $ elements ! ElDiskUsageRMaxTotal
    void $ updateElementValue (ElementDouble  $ nmDiskUsageW nm)              $ elements ! ElDiskUsageW
    void $ updateElementValue (ElementDouble  $ nmDiskUsageWMaxTotal nm)      $ elements ! ElDiskUsageWMaxTotal
    void $ updateElementValue (ElementDouble  $ nmNetworkUsageIn nm)          $ elements ! ElNetworkUsageIn
    void $ updateElementValue (ElementDouble  $ nmNetworkUsageInMaxTotal nm)  $ elements ! ElNetworkUsageInMaxTotal
    void $ updateElementValue (ElementDouble  $ nmNetworkUsageOut nm)         $ elements ! ElNetworkUsageOut
    void $ updateElementValue (ElementDouble  $ nmNetworkUsageOutMaxTotal nm) $ elements ! ElNetworkUsageOutMaxTotal
    void $ updateElementValue (ElementDouble  $ nmRTSMemoryAllocated nm)      $ elements ! ElRTSMemoryAllocated
    void $ updateElementValue (ElementDouble  $ nmRTSMemoryUsed nm)           $ elements ! ElRTSMemoryUsed
    void $ updateElementValue (ElementDouble  $ nmRTSMemoryUsedPercent nm)    $ elements ! ElRTSMemoryUsedPercent
    void $ updateElementValue (ElementDouble  $ nmRTSGcCpu nm)                $ elements ! ElRTSGcCpu
    void $ updateElementValue (ElementDouble  $ nmRTSGcElapsed nm)            $ elements ! ElRTSGcElapsed
    void $ updateElementValue (ElementInteger $ nmRTSGcNum nm)                $ elements ! ElRTSGcNum
    void $ updateElementValue (ElementInteger $ nmRTSGcMajorNum nm)           $ elements ! ElRTSGcMajorNum

    void $ updateProgressBar (nmMempoolBytesPercent nm)    $ elements ! ElMempoolBytesProgress
    void $ updateProgressBar (nmMempoolTxsPercent nm)      $ elements ! ElMempoolTxsProgress
    void $ updateProgressBar (nmMemoryPercent nm)          $ elements ! ElMemoryProgress
    void $ updateProgressBar (nmCPUPercent nm)             $ elements ! ElCPUProgress
    void $ updateProgressBar (nmDiskUsageRPercent nm)      $ elements ! ElDiskReadProgress
    void $ updateProgressBar (nmDiskUsageWPercent nm)      $ elements ! ElDiskWriteProgress
    void $ updateProgressBar (nmNetworkUsageInPercent nm)  $ elements ! ElNetworkInProgress
    void $ updateProgressBar (nmNetworkUsageOutPercent nm) $ elements ! ElNetworkOutProgress
    void $ updateProgressBar (nmRTSMemoryUsedPercent nm)   $ elements ! ElRTSMemoryProgress

    markOutdatedElements params ni nm elements

updateElementValue
  :: ElementValue
  -> Element
  -> UI Element
updateElementValue (ElementInteger i) el = element el # set text (show i)
updateElementValue (ElementWord64  w) el = element el # set text (show w)
updateElementValue (ElementDouble  d) el = element el # set text (showWith1DecPlace d)
updateElementValue (ElementString  s) el = element el # set text s

updateProgressBar
  :: Double
  -> Element
  -> UI Element
updateProgressBar percents bar =
  element bar # set style [("width", showWith1DecPlace preparedPercents <> "%")]
 where
  -- Sometimes (for CPU usage) percents can be bigger than 100%,
  -- in this case actual width of bar should be 100%.
  preparedPercents = if percents > 100.0 then 100.0 else percents

showWith1DecPlace :: Double -> String
showWith1DecPlace = unpack . sformat ("" % fixed 1)

updateNodeCommit
  :: String
  -> String
  -> Element
  -> UI Element
updateNodeCommit commit shortCommit commitHref = do
  sComm <- UI.string shortCommit
  element commitHref # set UI.href ("https://github.com/input-output-hk/cardano-node/commit/" <> commit)
                     # set children [sComm]

updateNodeUpTime
  :: Word64
  -> Element
  -> UI Element
updateNodeUpTime upTimeInNs upTimeLabel =
  element upTimeLabel # set text (formatTime defaultTimeLocale "%X" upTime)
 where
  upTimeInSec :: Double
  upTimeInSec = fromIntegral upTimeInNs / 1000000000
  -- We show up time as time with seconds, so we don't need fractions of second.
  upTimeDiff :: NominalDiffTime
  upTimeDiff = fromInteger $ round upTimeInSec
  upTime = addUTCTime upTimeDiff (UTCTime (ModifiedJulianDay 0) 0)

-- | Since peers list will be changed dynamically, we need it
--   to update corresponding HTML-murkup dynamically as well.
updatePeersList
  :: [PeerInfo]
  -> Element
  -> UI Element
updatePeersList peersInfo peersList = do
  peersItems <- forM peersInfo $ \(PeerInfo endpoint slotNo blockNo) ->
    UI.div #. "w3-row" #+
      [ UI.div #. "w3-third w3-theme" #+ [UI.div #. "" #+ [UI.string endpoint]]
      , UI.div #. "w3-third w3-theme" #+ [UI.div #. "" #+ [UI.string slotNo]]
      , UI.div #. "w3-third w3-theme" #+ [UI.div #. "" #+ [UI.string blockNo]]
      ]
  element peersList # set children peersItems

updateErrorsList
  :: [NodeError]
  -> Element
  -> UI Element
updateErrorsList nodeErrors errorsList = do
  errors <- forM nodeErrors $ \(NodeError timeStamp sev msg) -> do
    let className :: String
        className = case sev of
                      Warning   -> "warning-message"
                      Error     -> "error-message"
                      Critical  -> "critical-message"
                      Alert     -> "alert-message"
                      Emergency -> "emergency-message"
                      _         -> ""

    UI.div #. "w3-row" #+
      [ UI.div #. "w3-third w3-theme" #+ [UI.div #. "" #+ [UI.string (show timeStamp)]]
      , UI.div #. "w3-twothird w3-theme" #+ [UI.div #. className #+ [UI.string msg]]
      ]
  element errorsList # set children errors

-- | To show TraceAcceptorHost and TraceAcceptorPort
--   of the active node we use its name.
findTraceAcceptorNetInfo
  :: Text
  -> [RemoteAddrNamed]
  -> (String, String)
findTraceAcceptorNetInfo nameOfNode acceptors =
  case maybeActiveNode of
    Just (RemoteAddrNamed _ (RemoteSocket host port)) -> (host, port)
    Just (RemoteAddrNamed _ (RemotePipe _)) -> ("-", "-")
    Nothing -> ("-", "-")
 where
  maybeActiveNode = flip L.find acceptors $ \(RemoteAddrNamed name _) -> name == nameOfNode

-- | If some metric wasn't receive for a long time -
--   we mark corresponding value in GUI as outdated one.
markOutdatedElements
  :: RTViewParams
  -> NodeInfo
  -> NodeMetrics
  -> NodeStateElements
  -> UI ()
markOutdatedElements params ni nm els = do
  now <- liftIO $ getMonotonicTimeNSec
  -- Different metrics have different lifetime.
  let niLife  = rtvNodeInfoLife params
      bcLife  = rtvBlockchainInfoLife params
      resLife = rtvResourcesInfoLife params
      rtsLife = rtvRTSInfoLife params


  markValueW now (niUpTimeLastUpdate ni)       niLife [ els ! ElUptime
                                                      , els ! ElNodeRelease
                                                      , els ! ElNodeVersion
                                                      , els ! ElNodeCommitHref
                                                      ]
                                                      [ els ! ElUptimeOutdateWarning
                                                      , els ! ElNodeReleaseOutdateWarning
                                                      , els ! ElNodeVersionOutdateWarning
                                                      , els ! ElNodeCommitHrefOutdateWarning
                                                      ]

  markValue  now (niEpochLastUpdate ni)        bcLife (els ! ElEpoch)
  markValueW now (niSlotLastUpdate ni)         bcLife [els ! ElSlot]
                                                      [els ! ElSlotOutdateWarning]
  markValueW now (niBlocksNumberLastUpdate ni) bcLife [els ! ElBlocksNumber]
                                                      [els ! ElBlocksNumberOutdateWarning]
  markValueW now (niChainDensityLastUpdate ni) bcLife [els ! ElChainDensity]
                                                      [els ! ElChainDensityOutdateWarning]

  markValueW now (nmRTSGcCpuLastUpdate nm)      rtsLife [els ! ElRTSGcCpu]
                                                        [els ! ElRTSGcCpuOutdateWarning]
  markValueW now (nmRTSGcElapsedLastUpdate nm)  rtsLife [els ! ElRTSGcElapsed]
                                                        [els ! ElRTSGcElapsedOutdateWarning]
  markValueW now (nmRTSGcNumLastUpdate nm)      rtsLife [els ! ElRTSGcNum]
                                                        [els ! ElRTSGcNumOutdateWarning]
  markValueW now (nmRTSGcMajorNumLastUpdate nm) rtsLife [els ! ElRTSGcMajorNum]
                                                        [els ! ElRTSGcMajorNumOutdateWarning]

  -- Mark progress bars' state.
  markProgressBar now (nmRTSMemoryLastUpdate nm) rtsLife els ( ElRTSMemoryProgress
                                                             , ElRTSMemoryProgressBox
                                                             )
                                                             [ ElRTSMemoryAllocated
                                                             , ElRTSMemoryUsed
                                                             , ElRTSMemoryUsedPercent
                                                             ]
  markProgressBar now (nmMemoryLastUpdate nm) resLife els ( ElMemoryProgress
                                                          , ElMemoryProgressBox
                                                          )
                                                          [ ElMemoryMaxTotal
                                                          , ElMemory
                                                          , ElMemoryMax
                                                          ]
  markProgressBar now (nmCPULastUpdate nm)    resLife els ( ElCPUProgress
                                                          , ElCPUProgressBox
                                                          )
                                                          [ ElCPUPercent
                                                          ]
  markProgressBar now (nmDiskUsageRLastUpdate nm) resLife els ( ElDiskReadProgress
                                                              , ElDiskReadProgressBox
                                                              )
                                                              [ ElDiskUsageR
                                                              , ElDiskUsageRMaxTotal
                                                              ]
  markProgressBar now (nmDiskUsageWLastUpdate nm) resLife els ( ElDiskWriteProgress
                                                              , ElDiskWriteProgressBox
                                                              )
                                                              [ ElDiskUsageW
                                                              , ElDiskUsageWMaxTotal
                                                              ]
  markProgressBar now (nmNetworkUsageInLastUpdate nm)  resLife els ( ElNetworkInProgress
                                                                   , ElNetworkInProgressBox
                                                                   )
                                                                   [ ElNetworkUsageIn
                                                                   , ElNetworkUsageInMaxTotal
                                                                   ]
  markProgressBar now (nmNetworkUsageOutLastUpdate nm) resLife els ( ElNetworkOutProgress
                                                                   , ElNetworkOutProgressBox
                                                                   )
                                                                   [ ElNetworkUsageOut
                                                                   , ElNetworkUsageOutMaxTotal
                                                                   ]

markValue
  :: Word64
  -> Word64
  -> Word64
  -> Element
  -> UI ()
markValue now lastUpdate lifetime el =
  if now - lastUpdate > lifetime
    then void $ markAsOutdated el
    else void $ markAsUpToDate el

markValueW
  :: Word64
  -> Word64
  -> Word64
  -> [Element]
  -> [Element]
  -> UI ()
markValueW now lastUpdate lifetime els warnings =
  if now - lastUpdate > lifetime
    then do
      mapM_ (void . markAsOutdated) els
      mapM_ (void . showWarning) warnings
    else do
      mapM_ (void . markAsUpToDate) els
      mapM_ (void . hideWarning) warnings

showWarning, hideWarning :: Element -> UI Element
showWarning w = element w # set UI.style [("display", "inline")]
hideWarning w = element w # set UI.style [("display", "none")]

markAsOutdated, markAsUpToDate :: Element -> UI Element
markAsOutdated el = element el # set UI.class_ "outdated-value"
markAsUpToDate el = element el # set UI.class_ ""

markProgressBar
  :: Word64
  -> Word64
  -> Word64
  -> NodeStateElements
  -> (ElementName, ElementName)
  -> [ElementName]
  -> UI ()
markProgressBar now lastUpdate lifetime els (barName, barBoxName) labelsNames =
  if now - lastUpdate > lifetime
    then markBarAsOutdated
    else markBarAsUpToDate
 where
  bar    = els ! barName
  barBox = els ! barBoxName

  barClass, barBoxClass :: String
  barClass = show barName
  barBoxClass = show barBoxName

  barClassOutdated, barBoxClassOutdated :: String
  barClassOutdated = barClass <> "-outdated"
  barBoxClassOutdated = barBoxClass <> "-outdated"

  markBarAsOutdated = do
    void $ element bar    # set UI.class_ barClassOutdated
    void $ element barBox # set UI.class_ barBoxClassOutdated
                          # set UI.title__ "The progress values are outdated"
    forM_ labelsNames $ \name -> void . markAsOutdated $ els ! name
  markBarAsUpToDate = do
    void $ element bar    # set UI.class_ barClass
    void $ element barBox # set UI.class_ barBoxClass
                          # set UI.title__ ""
    forM_ labelsNames $ \name -> void . markAsUpToDate $ els ! name
