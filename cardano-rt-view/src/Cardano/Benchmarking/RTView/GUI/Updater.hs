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
                   ( UTCTime (..)
                   , addUTCTime, diffUTCTime, getCurrentTime
                   )
import           Data.Time.Format
                   ( defaultTimeLocale, formatTime )
import           Data.Text
                   ( unpack )
import           Formatting
                   ( sformat, fixed, (%) )
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
import           Cardano.Benchmarking.RTView.GUI.Elements
                   ( ElementName (..), ElementValue (..)
                   , NodesStateElements
                   )
import           Cardano.Benchmarking.RTView.NodeState.Types
                   ( NodeInfo (..), NodeMetrics (..), NodeError (..)
                   , NodeState (..), NodesState, PeerInfo (..)
                   )

-- | This function is calling by the timer. It updates the node' state elements
--   on the page automatically, because threepenny-gui is based on websockets.
updateGUI
  :: NodesState
  -> [RemoteAddrNamed]
  -> NodesStateElements
  -> UI ()
updateGUI nodesState acceptors nodesStateElems =
  forM_ nodesStateElems $ \(nameOfNode, elements) -> do
    let nodeState = nodesState ! nameOfNode
        (acceptorHost, acceptorPort) = findTraceAcceptorNetInfo nameOfNode acceptors

    let ni = nsInfo nodeState
        nm = nsMetrics nodeState

    now <- liftIO getCurrentTime
    let diffBetweenNowAndStart = diffUTCTime now (niStartTime ni)
        upTimeHMS = formatTime defaultTimeLocale "%X" $
                      addUTCTime diffBetweenNowAndStart (UTCTime (ModifiedJulianDay 0) 0)
        activeNodeMark = unpack nameOfNode

    void $ updateElementValue (ElementString  $ niNodeRelease ni)             $ elements ! ElNodeRelease
    void $ updateElementValue (ElementString  $ niNodeVersion ni)             $ elements ! ElNodeVersion
    void $ updateElementValue (ElementString  $ niNodeCommit ni)              $ elements ! ElNodeCommit
    void $ updateElementValue (ElementString  $ niNodeShortCommit ni)         $ elements ! ElNodeShortCommit
    void $ updateElementValue (ElementString activeNodeMark)                  $ elements ! ElActiveNode
    void $ updateElementValue (ElementString upTimeHMS)                       $ elements ! ElUptime
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
