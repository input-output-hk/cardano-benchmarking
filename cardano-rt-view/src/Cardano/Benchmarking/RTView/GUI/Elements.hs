{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Benchmarking.RTView.GUI.Elements
    ( NodesStateElements
    , NodeStateElements
    , ElementName (..)
    , ElementValue (..)
    , PeerInfoItem (..)
    , PeerInfoElements (..)
    ) where

import           Cardano.Prelude
import           Control.DeepSeq (NFData (..), rwhnf)
import           Data.Map.Strict (Map)
import           Prelude (String)

import           Graphics.UI.Threepenny.Core (Element)

instance NFData Element where
  rnf = rwhnf

-- | GUI elements containing current node state (info, metrics).
--   These elements are continuously updating using |LogObject|s
--   received by |TraceAcceptor|s.
type NodeStateElements = Map ElementName Element

-- | GUI elements for all nodes, tuples from nodeName, its elements and prepared peers items.
type NodesStateElements = [(Text, NodeStateElements, [PeerInfoItem])]

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
  | ElNodeCannotLead
  | ElChainDensity
  | ElNodeIsLeaderNumber
  | ElSlotsMissedNumber
  | ElTxsProcessed
  | ElPeersNumber
  | ElTraceAcceptorHost
  | ElTraceAcceptorPort
  | ElTraceAcceptorEndpoint
  | ElOpCertStartKESPeriod
  | ElCurrentKESPeriod
  | ElRemainingKESPeriods
  | ElNodeErrors
  | ElMempoolTxsNumber
  | ElMempoolTxsPercent
  | ElMempoolBytes
  | ElMempoolBytesPercent
  | ElMempoolMaxTxs
  | ElMempoolMaxBytes
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
  | ElOpCertStartKESPeriodOutdateWarning
  | ElCurrentKESPeriodOutdateWarning
  | ElRemainingKESPeriodsOutdateWarning
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
  -- Charts
  | ElMemoryUsageChart
  | ElCPUUsageChart
  | ElDiskUsageChart
  | ElNetworkUsageChart
  deriving (Eq, Generic, NFData, Ord, Show)

data ElementValue
  = ElementInteger !Integer
  | ElementWord64  !Word64
  | ElementDouble  !Double
  | ElementString  !String
  deriving (Generic, NFData)

-- | An item for each connected peer, contains a parent element
--   and list of child elements.
data PeerInfoItem
  = PeerInfoItem
      { piItem      :: !Element
      , piItemElems :: !PeerInfoElements
      }
  deriving (Generic, NFData)

data PeerInfoElements
  = PeerInfoElements
      { pieEndpoint   :: !Element
      , pieBytesInF   :: !Element
      , pieReqsInF    :: !Element
      , pieBlocksInF  :: !Element
      , pieSlotNumber :: !Element
      , pieStatus     :: !Element
      }
  deriving (Generic, NFData)
