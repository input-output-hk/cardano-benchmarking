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
  let nodeState = nodesState ! "a" -- TODO: fix it!

  let ni = nsInfo nodeState
      nm = nsMetrics nodeState
    
  now <- liftIO getCurrentTime
  let diffBetweenNowAndStart = diffUTCTime now (niStartTime ni)
      upTimeHMS = formatTime defaultTimeLocale "%X" $
                    addUTCTime diffBetweenNowAndStart (UTCTime (ModifiedJulianDay 0) 0)
      mempoolTxsCapacity' :: Double
      mempoolTxsCapacity' = fromIntegral $ nmMempoolTxsCapacity nm

  void $ updateElementValue (ElementString upTimeHMS)                 $ elements ! ElUptime
  void $ updateElementValue (ElementInt    $ niEpoch ni)              $ elements ! ElEpoch
  void $ updateElementValue (ElementInt    $ niSlot ni)               $ elements ! ElSlot
  void $ updateElementValue (ElementInt    $ niBlocksNumber ni)       $ elements ! ElBlocksNumber
  void $ updateElementValue (ElementDouble $ niChainDensity ni)       $ elements ! ElChainDensity
  void $ updateElementValue (ElementInt    $ niTxsProcessed ni)       $ elements ! ElTxsProcessed
  void $ updateElementValue (ElementString $ niTraceAcceptorHost ni)  $ elements ! ElTraceAcceptorHost
  void $ updateElementValue (ElementInt    $ niTraceAcceptorPort ni)  $ elements ! ElTraceAcceptorPort
  void $ updateElementValue (ElementDouble $ nmMempoolKBMax nm)       $ elements ! ElMempoolKBMax
  void $ updateElementValue (ElementDouble $ nmMempoolKB nm)          $ elements ! ElMempoolKB
  void $ updateElementValue (ElementDouble $ nmMempoolKBPercent nm)   $ elements ! ElMempoolKBPercent
  void $ updateElementValue (ElementInt    $ nmMempoolTxsCapacity nm) $ elements ! ElMempoolTxsCapacity
  void $ updateElementValue (ElementInt    $ nmMempoolTxsNumber nm)   $ elements ! ElMempoolTxsNumber
  void $ updateElementValue (ElementDouble $ nmMempoolTxsPercent nm)  $ elements ! ElMempoolTxsPercent
  void $ updateElementValue (ElementDouble $ nmMemoryMax nm)          $ elements ! ElMemoryMax
  void $ updateElementValue (ElementDouble $ nmMemoryPercent nm)      $ elements ! ElMemoryPercent
  void $ updateElementValue (ElementDouble $ nmCPUMax nm)             $ elements ! ElCPUMax
  void $ updateElementValue (ElementDouble $ nmCPUPercent nm)         $ elements ! ElCPUPercent
  void $ updateElementValue (ElementDouble $ nmDiskReadMax nm)        $ elements ! ElDiskReadMax
  void $ updateElementValue (ElementDouble $ nmDiskReadPercent nm)    $ elements ! ElDiskReadPercent
  void $ updateElementValue (ElementDouble $ nmDiskWriteMax nm)       $ elements ! ElDiskWriteMax
  void $ updateElementValue (ElementDouble $ nmDiskWritePercent nm)   $ elements ! ElDiskWritePercent
  void $ updateElementValue (ElementDouble $ nmNetworkInMax nm)       $ elements ! ElNetworkInMax
  void $ updateElementValue (ElementDouble $ nmNetworkInPercent nm)   $ elements ! ElNetworkInPercent
  void $ updateElementValue (ElementDouble $ nmNetworkOutMax nm)      $ elements ! ElNetworkOutMax
  void $ updateElementValue (ElementDouble $ nmNetworkOutPercent nm)  $ elements ! ElNetworkOutPercent
  void $ updateElementValue (ElementDouble $ nmNetworkOutPercent nm)  $ elements ! ElNetworkOutPercent

  void $ updateProgressBar (nmMempoolKBPercent nm)  (nmMempoolKBMax nm)   $ elements ! ElMempoolKBProgress
  void $ updateProgressBar (nmMempoolTxsPercent nm) (mempoolTxsCapacity') $ elements ! ElMempoolTxsProgress
  void $ updateProgressBar (nmMemoryPercent nm)     (nmMemoryMax nm)      $ elements ! ElMemoryProgress
  void $ updateProgressBar (nmCPUPercent nm)        (nmCPUMax nm)         $ elements ! ElCPUProgress
  void $ updateProgressBar (nmDiskReadPercent nm)   (nmDiskReadMax nm)    $ elements ! ElDiskReadProgress
  void $ updateProgressBar (nmDiskWritePercent nm)  (nmDiskWriteMax nm)   $ elements ! ElDiskWriteProgress
  void $ updateProgressBar (nmNetworkInPercent nm)  (nmNetworkInMax nm)   $ elements ! ElNetworkInProgress
  void $ updateProgressBar (nmNetworkOutPercent nm) (nmNetworkOutMax nm)  $ elements ! ElNetworkOutProgress

updateElementValue
  :: ElementValue
  -> Element
  -> UI Element
updateElementValue (ElementInt    i) el = element el # set text (show i)
updateElementValue (ElementDouble d) el = element el # set text (showDoubleWith1DecPlace d)
updateElementValue (ElementString s) el = element el # set text s

updateProgressBar
  :: Double
  -> Double
  -> Element
  -> UI Element
updateProgressBar value maxValue bar = do
  let onePercent     = maxValue / 100.0
      percents       = value / onePercent
  element bar # set style [("width", showDoubleWith1DecPlace percents <> "%")]
  
showDoubleWith1DecPlace :: Double -> String
showDoubleWith1DecPlace = unpack . sformat ("" % fixed 1)