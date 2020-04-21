{-# LANGUAGE OverloadedStrings #-}

module Cardano.Benchmarking.RTView.GUI.Elements
    ( NodesStateElements
    , NodeStateElements
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

-- | GUI elements for all nodes, pairs from nodeName and its elements.
type NodesStateElements = [(Text, NodeStateElements)]

data ElementName
  = ElNodeRelease
  | ElNodeVersion
  | ElNodeCommit
  | ElNodeShortCommit
  | ElActiveNode
  | ElUptime
  | ElEpoch
  | ElSlot
  | ElBlocksNumber
  | ElChainDensity
  | ElTxsProcessed
  | ElPeersNumber
  | ElPeersList
  | ElTraceAcceptorHost
  | ElTraceAcceptorPort
  | ElNodeErrors
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
  | ElRTSMemoryAllocated
  | ElRTSMemoryUsed
  | ElRTSMemoryUsedPercent
  | ElRTSGcCpu
  | ElRTSGcElapsed
  | ElRTSGcNum
  | ElRTSGcMajorNum
  -- Progress bars.
  | ElMempoolBytesProgress
  | ElMempoolTxsProgress
  | ElMemoryProgress
  | ElCPUProgress
  | ElDiskReadProgress
  | ElDiskWriteProgress
  | ElNetworkInProgress
  | ElNetworkOutProgress
  | ElRTSMemoryProgress
  deriving (Eq, Ord)

data ElementValue
  = ElementInteger Integer
  | ElementWord64  Word64
  | ElementDouble  Double
  | ElementString  String
