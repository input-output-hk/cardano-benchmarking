{-# LANGUAGE OverloadedStrings #-}

module Cardano.Benchmarking.RTView.GUI.Elements
    ( NodeStateElements
    , ElementName (..)
    , ElementValue (..)
    ) where

import           Cardano.Prelude
import           Prelude
                   ( String )
import           Data.Map.Strict
                   ( Map )

import           Graphics.UI.Threepenny.Core
                   ( Element )

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
  | ElTraceAcceptorHost
  | ElTraceAcceptorPort
  | ElMempoolKBMax
  | ElMempoolKB
  | ElMempoolKBPercent
  | ElMempoolTxsCapacity
  | ElMempoolTxsNumber
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
