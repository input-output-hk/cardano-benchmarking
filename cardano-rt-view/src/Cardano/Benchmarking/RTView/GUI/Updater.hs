{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.GUI.Updater
    ( updateGUI
    ) where

import           Cardano.Prelude hiding ( (%) )
import           Prelude
                   ( String )
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
import           Graphics.UI.Threepenny.Core
                   ( Element, UI
                   , (#), element, set, style, text
                   )

import           Cardano.Benchmarking.RTView.GUI.Elements
                   ( ElementName (..), ElementValue (..)
                   , NodeStateElements
                   )
import           Cardano.Benchmarking.RTView.NodeState.Types
                   ( NodeInfo (..), NodeMetrics (..)
                   , NodeState (..), NodesState
                   )

-- | This function is calling by the timer. It updates the node' state elements
--   on the page automatically, because threepenny-gui is based on websockets.
updateGUI
  :: NodesState
  -> NodeStateElements
  -> UI ()
updateGUI nodesState elements = do
  -- TODO: Currently, we test it with one node only,
  -- later selector will be added!
  let nodeState = nodesState ! "a"

  let ni = nsInfo nodeState
      nm = nsMetrics nodeState
    
  now <- liftIO getCurrentTime
  let diffBetweenNowAndStart = diffUTCTime now (niStartTime ni)
      upTimeHMS = formatTime defaultTimeLocale "%X" $
                    addUTCTime diffBetweenNowAndStart (UTCTime (ModifiedJulianDay 0) 0)

  void $ updateElementValue (ElementString upTimeHMS)                      $ elements ! ElUptime
  void $ updateElementValue (ElementInt    $ niEpoch ni)                   $ elements ! ElEpoch
  void $ updateElementValue (ElementInt    $ niSlot ni)                    $ elements ! ElSlot
  void $ updateElementValue (ElementInt    $ niBlocksNumber ni)            $ elements ! ElBlocksNumber
  void $ updateElementValue (ElementDouble $ niChainDensity ni)            $ elements ! ElChainDensity
  void $ updateElementValue (ElementInt    $ niTxsProcessed ni)            $ elements ! ElTxsProcessed
  void $ updateElementValue (ElementInt    $ niPeersNumber ni)             $ elements ! ElPeersNumber
  void $ updateElementValue (ElementString $ niTraceAcceptorHost ni)       $ elements ! ElTraceAcceptorHost
  void $ updateElementValue (ElementInt    $ niTraceAcceptorPort ni)       $ elements ! ElTraceAcceptorPort
  void $ updateElementValue (ElementWord64 $ nmMempoolTxsNumber nm)        $ elements ! ElMempoolTxsNumber
  void $ updateElementValue (ElementDouble $ nmMempoolTxsPercent nm)       $ elements ! ElMempoolTxsPercent
  void $ updateElementValue (ElementWord64 $ nmMempoolBytes nm)            $ elements ! ElMempoolBytes
  void $ updateElementValue (ElementDouble $ nmMempoolBytesPercent nm)     $ elements ! ElMempoolBytesPercent
  void $ updateElementValue (ElementWord64 $ nmMempoolCapacity nm)         $ elements ! ElMempoolCapacity
  void $ updateElementValue (ElementWord64 $ nmMempoolCapacityBytes nm)    $ elements ! ElMempoolCapacityBytes
  void $ updateElementValue (ElementDouble $ nmMemory nm)                  $ elements ! ElMemory
  void $ updateElementValue (ElementDouble $ nmMemoryMax nm)               $ elements ! ElMemoryMax
  void $ updateElementValue (ElementDouble $ nmMemoryMaxTotal nm)          $ elements ! ElMemoryMaxTotal
  void $ updateElementValue (ElementDouble $ nmMemoryPercent nm)           $ elements ! ElMemoryPercent
  void $ updateElementValue (ElementDouble $ nmCPUPercent nm)              $ elements ! ElCPUPercent
  void $ updateElementValue (ElementDouble $ nmDiskUsageR nm)              $ elements ! ElDiskUsageR
  void $ updateElementValue (ElementDouble $ nmDiskUsageRMaxTotal nm)      $ elements ! ElDiskUsageRMaxTotal
  void $ updateElementValue (ElementDouble $ nmDiskUsageW nm)              $ elements ! ElDiskUsageW
  void $ updateElementValue (ElementDouble $ nmDiskUsageWMaxTotal nm)      $ elements ! ElDiskUsageWMaxTotal
  void $ updateElementValue (ElementDouble $ nmNetworkUsageIn nm)          $ elements ! ElNetworkUsageIn
  void $ updateElementValue (ElementDouble $ nmNetworkUsageInMaxTotal nm)  $ elements ! ElNetworkUsageInMaxTotal
  void $ updateElementValue (ElementDouble $ nmNetworkUsageOut nm)         $ elements ! ElNetworkUsageOut
  void $ updateElementValue (ElementDouble $ nmNetworkUsageOutMaxTotal nm) $ elements ! ElNetworkUsageOutMaxTotal

  void $ updateProgressBar (nmMempoolBytesPercent nm)    $ elements ! ElMempoolBytesProgress
  void $ updateProgressBar (nmMempoolTxsPercent nm)      $ elements ! ElMempoolTxsProgress
  void $ updateProgressBar (nmMemoryPercent nm)          $ elements ! ElMemoryProgress
  void $ updateProgressBar (nmCPUPercent nm)             $ elements ! ElCPUProgress
  void $ updateProgressBar (nmDiskUsageRPercent nm)      $ elements ! ElDiskReadProgress
  void $ updateProgressBar (nmDiskUsageWPercent nm)      $ elements ! ElDiskWriteProgress
  void $ updateProgressBar (nmNetworkUsageInPercent nm)  $ elements ! ElNetworkInProgress
  void $ updateProgressBar (nmNetworkUsageOutPercent nm) $ elements ! ElNetworkOutProgress

updateElementValue
  :: ElementValue
  -> Element
  -> UI Element
updateElementValue (ElementInt     i) el = element el # set text (show i)
updateElementValue (ElementInteger i) el = element el # set text (show i)
updateElementValue (ElementWord64  w) el = element el # set text (show w)
updateElementValue (ElementDouble  d) el = element el # set text (showDoubleWith1DecPlace d)
updateElementValue (ElementString  s) el = element el # set text s

updateProgressBar
  :: Double
  -> Element
  -> UI Element
updateProgressBar percents bar = do
  element bar # set style [("width", showDoubleWith1DecPlace percents <> "%")]

showDoubleWith1DecPlace :: Double -> String
showDoubleWith1DecPlace = unpack . sformat ("" % fixed 1)
