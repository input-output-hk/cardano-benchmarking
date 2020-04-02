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
  | ElPeersNumber
  | ElTraceAcceptorHost
  | ElTraceAcceptorPort
  | ElMempoolTxsNumber
  | ElMempoolTxsPercent
  | ElMempoolBytes
  | ElMempoolBytesPercent
  | ElMempoolCapacity
  | ElMempoolCapacityBytes
  | ElMemory
  | ElMemoryMax
  | ElMemoryMaxTotal
  | ElMemoryPercent
  | ElCPUPercent
  | ElCPULast
  | ElCPUNs
  | ElDiskUsageR
  | ElDiskUsageRMax
  | ElDiskUsageRMaxTotal
  | ElDiskUsageRPercent
  | ElDiskUsageW
  | ElDiskUsageWMax
  | ElDiskUsageWMaxTotal
  | ElDiskUsageWPercent
  | ElNetworkUsageIn
  | ElNetworkUsageInMaxTotal
  | ElNetworkUsageOut
  | ElNetworkUsageOutMaxTotal
  -- Progress bars.
  | ElMempoolBytesProgress
  | ElMempoolTxsProgress
  | ElMemoryProgress
  | ElCPUProgress
  | ElDiskReadProgress
  | ElDiskWriteProgress
  | ElNetworkInProgress
  | ElNetworkOutProgress
  deriving (Eq, Ord)

data ElementValue
  = ElementInt     Int
  | ElementInteger Integer
  | ElementWord64  Word64
  | ElementDouble  Double
  | ElementString  String
