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
  | ElNodePlatform
  | ElNodeCommitHref
  | ElActiveNode
  | ElUptime
  | ElEpoch
  | ElSlot
  | ElBlocksNumber
  | ElBlocksForgedNumber
  | ElChainDensity
  | ElNodeIsLeaderNumber
  | ElSlotsMissedNumber
  | ElForksCreatedNumber
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
  -- Outdated warnings.
  | ElNodeReleaseOutdateWarning
  | ElNodeVersionOutdateWarning
  | ElNodePlatformOutdateWarning
  | ElNodeCommitHrefOutdateWarning
  | ElUptimeOutdateWarning
  | ElSlotOutdateWarning
  | ElBlocksNumberOutdateWarning
  | ElBlocksForgedNumberOutdateWarning
  | ElChainDensityOutdateWarning
  | ElNodeIsLeaderNumberOutdateWarning
  | ElSlotsMissedNumberOutdateWarning
  | ElForksCreatedNumberOutdateWarning
  | ElRTSGcCpuOutdateWarning
  | ElRTSGcElapsedOutdateWarning
  | ElRTSGcNumOutdateWarning
  | ElRTSGcMajorNumOutdateWarning
  -- Progress bars.
  | ElMempoolBytesProgress
  | ElMempoolBytesProgressBox
  | ElMempoolTxsProgress
  | ElMempoolTxsProgressBox
  | ElMemoryProgress
  | ElMemoryProgressBox
  | ElCPUProgress
  | ElCPUProgressBox
  | ElDiskReadProgress
  | ElDiskReadProgressBox
  | ElDiskWriteProgress
  | ElDiskWriteProgressBox
  | ElNetworkInProgress
  | ElNetworkInProgressBox
  | ElNetworkOutProgress
  | ElNetworkOutProgressBox
  | ElRTSMemoryProgress
  | ElRTSMemoryProgressBox
  deriving (Eq, Ord, Show)

data ElementValue
  = ElementInteger Integer
  | ElementWord64  Word64
  | ElementDouble  Double
  | ElementString  String
