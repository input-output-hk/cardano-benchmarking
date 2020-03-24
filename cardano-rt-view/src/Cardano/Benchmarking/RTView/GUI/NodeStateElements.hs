{-# LANGUAGE OverloadedStrings #-}

module Cardano.Benchmarking.RTView.GUI.NodeStateElements
    ( NodeStateElements
    , ElementName (..)
    , ElementValue (..)
    , updateElementValue
    , updateProgressBar
    ) where

import           Cardano.Prelude hiding ( (%) )
import           Prelude
                   ( String )
import           Data.Map.Strict 
                   ( Map )
import           Data.Text
                   ( unpack )
import           Formatting
                   ( sformat, fixed, (%) )

import           Graphics.UI.Threepenny.Core
                   ( Element, UI
                   , (#), element, set, style, text
                   )

-- | GUI elements containing current node state (info, metrics).
--   These elements are continuously updating using |LogObject|s
--   received by |TraceAcceptor|s.
type NodeStateElements = Map ElementName Element

data ElementName
  = ElUptime
  | ElEpoch
  | ElSlot
  | ElBlocksNumber
  | ElChainDensity
  | ElTxsProcessed
  | ElPort
  | ElPeersNumber
  | ElMempoolKBMax
  | ElMempoolKBPercent
  | ElMempoolTxsMax
  | ElMempoolTxsPercent
  | ElMemoryMax
  | ElMemoryPercent
  | ElCPUMax
  | ElCPUPercent
  | ElDiskReadMax
  | ElDiskReadPercent
  | ElDiskWriteMax
  | ElDiskWritePercent
  | ElNetworkInMax
  | ElNetworkInPercent
  | ElNetworkOutMax
  | ElNetworkOutPercent
  | ElMempoolKBProgress
  | ElMempoolTxsProgress
  | ElMemoryProgress
  | ElCPUProgress
  | ElDiskReadProgress
  | ElDiskWriteProgress
  | ElNetworkInProgress
  | ElNetworkOutProgress
  deriving (Eq, Ord)

data ElementValue
  = ElementInt    Int
  | ElementDouble Double
  | ElementString String

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
