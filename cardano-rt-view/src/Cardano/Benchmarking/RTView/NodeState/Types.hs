{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.NodeState.Types
    ( NodeError (..)
    , NodesState
    , NodeState (..)
    , NodeInfo (..)
    , NodeMetrics (..)
    , PeerInfo (..)
    , defaultNodesState
    ) where

import           Cardano.Prelude
import           Prelude
                   ( String )
import           Control.DeepSeq
                   ( NFData (..) )
import qualified Data.Map.Strict as Map
import           Data.Map.Strict
                   ( Map )
import           Data.Time.Calendar
                   ( Day (..) )
import           Data.Time.Clock
                   ( UTCTime (..) )

import           Cardano.BM.Configuration
                   ( Configuration )
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Severity
                   ( Severity )
import           Cardano.BM.Data.Configuration
                   ( RemoteAddrNamed (..) )

type NodesState = Map Text NodeState

data NodeState = NodeState
  { nsInfo    :: !NodeInfo
  , nsMetrics :: !NodeMetrics
  } deriving (Generic, NFData, Show)

data PeerInfo = PeerInfo
  { piEndpoint   :: !String
  , piBytesInF   :: !String
  , piReqsInF    :: !String
  , piBlocksInF  :: !String
  , piSlotNumber :: !String
  , piStatus     :: !String
  } deriving (Eq, Generic, NFData, Show)

-- Severity type already has Generic instance.
instance NFData Severity

data NodeError = NodeError
  { eTimestamp :: !UTCTime
  , eSeverity  :: !Severity
  , eMessage   :: !String
  } deriving (Generic, NFData, Show)

data NodeInfo = NodeInfo
  { niNodeRelease                   :: !String
  , niNodeVersion                   :: !String
  , niNodeCommit                    :: !String
  , niNodeShortCommit               :: !String
  , niNodePlatform                  :: !String
  , niUpTime                        :: !Word64
  , niUpTimeLastUpdate              :: !Word64
  , niEpoch                         :: !Integer
  , niEpochLastUpdate               :: !Word64
  , niSlot                          :: !Integer
  , niSlotLastUpdate                :: !Word64
  , niNodeIsLeaderNum               :: !Integer
  , niNodeIsLeaderNumLastUpdate     :: !Word64
  , niSlotsMissedNumber             :: !Integer
  , niSlotsMissedNumberLastUpdate   :: !Word64
  , niBlocksNumber                  :: !Integer
  , niBlocksNumberLastUpdate        :: !Word64
  , niBlocksForgedNumber            :: !Integer
  , niBlocksForgedNumberLastUpdate  :: !Word64
  , niNodeCannotLead                :: !Integer
  , niChainDensity                  :: !Double
  , niChainDensityLastUpdate        :: !Word64
  , niTxsProcessed                  :: !Integer
  , niPeersNumber                   :: !Integer
  , niPeersInfo                     :: ![PeerInfo]
  , niTraceAcceptorHost             :: !String
  , niTraceAcceptorPort             :: !String
  , niNodeErrors                    :: ![NodeError]
  } deriving (Generic, NFData, Show)

data NodeMetrics = NodeMetrics
  { nmMempoolTxsNumber          :: !Word64
  , nmMempoolTxsPercent         :: !Double
  , nmMempoolBytes              :: !Word64
  , nmMempoolBytesPercent       :: !Double
  , nmMempoolMaxTxs             :: !Integer
  , nmMempoolMaxBytes           :: !Integer
  , nmMemory                    :: !Double
  , nmMemoryMax                 :: !Double
  , nmMemoryMaxTotal            :: !Double
  , nmMemoryPercent             :: !Double
  , nmMemoryLastUpdate          :: !Word64
  , nmCPUPercent                :: !Double
  , nmCPULast                   :: !Integer
  , nmCPUNs                     :: !Word64
  , nmCPULastUpdate             :: !Word64
  , nmDiskUsageR                :: !Double
  , nmDiskUsageRMax             :: !Double
  , nmDiskUsageRMaxTotal        :: !Double
  , nmDiskUsageRPercent         :: !Double
  , nmDiskUsageRLast            :: !Word64
  , nmDiskUsageRNs              :: !Word64
  , nmDiskUsageRAdaptTime       :: !UTCTime
  , nmDiskUsageRLastUpdate      :: !Word64
  , nmDiskUsageW                :: !Double
  , nmDiskUsageWMax             :: !Double
  , nmDiskUsageWMaxTotal        :: !Double
  , nmDiskUsageWPercent         :: !Double
  , nmDiskUsageWLast            :: !Word64
  , nmDiskUsageWNs              :: !Word64
  , nmDiskUsageWAdaptTime       :: !UTCTime
  , nmDiskUsageWLastUpdate      :: !Word64
  , nmNetworkUsageIn            :: !Double
  , nmNetworkUsageInPercent     :: !Double
  , nmNetworkUsageInMax         :: !Double
  , nmNetworkUsageInMaxTotal    :: !Double
  , nmNetworkUsageInLast        :: !Word64
  , nmNetworkUsageInNs          :: !Word64
  , nmNetworkUsageInLastUpdate  :: !Word64
  , nmNetworkUsageOut           :: !Double
  , nmNetworkUsageOutPercent    :: !Double
  , nmNetworkUsageOutMax        :: !Double
  , nmNetworkUsageOutMaxTotal   :: !Double
  , nmNetworkUsageOutLast       :: !Word64
  , nmNetworkUsageOutNs         :: !Word64
  , nmNetworkUsageOutLastUpdate :: !Word64
  , nmRTSMemoryAllocated        :: !Double
  , nmRTSMemoryUsed             :: !Double
  , nmRTSMemoryUsedPercent      :: !Double
  , nmRTSMemoryLastUpdate       :: !Word64
  , nmRTSGcCpu                  :: !Double
  , nmRTSGcCpuLastUpdate        :: !Word64
  , nmRTSGcElapsed              :: !Double
  , nmRTSGcElapsedLastUpdate    :: !Word64
  , nmRTSGcNum                  :: !Integer
  , nmRTSGcNumLastUpdate        :: !Word64
  , nmRTSGcMajorNum             :: !Integer
  , nmRTSGcMajorNumLastUpdate   :: !Word64
  } deriving (Generic, NFData, Show)

defaultNodesState
  :: Configuration
  -> IO NodesState
defaultNodesState config =
  CM.getAcceptAt config >>= \case
    Just remoteAddresses -> do
      return $ Map.fromList [(name, defaultNodeState) | (RemoteAddrNamed name _) <- remoteAddresses]
    Nothing ->
      -- Actually it's impossible, because at this point we already know
      -- that at least one |TraceAcceptor| is defined in the config.
      return Map.empty

defaultNodeState :: NodeState
defaultNodeState = NodeState
  { nsInfo    = defaultNodeInfo
  , nsMetrics = defaultNodeMetrics
  }

defaultNodeInfo :: NodeInfo
defaultNodeInfo = NodeInfo
  { niNodeRelease                   = "—"
  , niNodeVersion                   = "—"
  , niNodeCommit                    = "—"
  , niNodeShortCommit               = "—"
  , niNodePlatform                  = "—"
  , niUpTime                        = 0
  , niUpTimeLastUpdate              = 0
  , niEpoch                         = 0
  , niEpochLastUpdate               = 0
  , niSlot                          = 0
  , niSlotLastUpdate                = 0
  , niNodeIsLeaderNum               = 0
  , niNodeIsLeaderNumLastUpdate     = 0
  , niSlotsMissedNumber             = 0
  , niSlotsMissedNumberLastUpdate   = 0
  , niBlocksNumber                  = 0
  , niBlocksNumberLastUpdate        = 0
  , niBlocksForgedNumber            = 0
  , niBlocksForgedNumberLastUpdate  = 0
  , niNodeCannotLead                = 0
  , niChainDensity                  = 0.0
  , niChainDensityLastUpdate        = 0
  , niTxsProcessed                  = 0
  , niPeersNumber                   = 0
  , niPeersInfo                     = []
  , niTraceAcceptorHost             = "-"
  , niTraceAcceptorPort             = "-"
  , niNodeErrors                    = []
  }

defaultNodeMetrics :: NodeMetrics
defaultNodeMetrics = NodeMetrics
  { nmMempoolTxsNumber          = 0
  , nmMempoolTxsPercent         = 0.0
  , nmMempoolBytes              = 0
  , nmMempoolBytesPercent       = 0.0
  , nmMempoolMaxTxs             = 0
  , nmMempoolMaxBytes           = 0
  , nmMemory                    = 0.0
  , nmMemoryMax                 = 0.0
  , nmMemoryMaxTotal            = 200.0
  , nmMemoryPercent             = 0.0
  , nmMemoryLastUpdate          = 0
  , nmCPUPercent                = 0.5
  , nmCPULast                   = 0
  , nmCPUNs                     = 10000
  , nmCPULastUpdate             = 0
  , nmDiskUsageR                = 0.0
  , nmDiskUsageRMax             = 0.0
  , nmDiskUsageRMaxTotal        = 0.0
  , nmDiskUsageRPercent         = 0.0
  , nmDiskUsageRLast            = 0
  , nmDiskUsageRNs              = 10000
  , nmDiskUsageRAdaptTime       = UTCTime (ModifiedJulianDay 0) 0
  , nmDiskUsageRLastUpdate      = 0
  , nmDiskUsageW                = 0.0
  , nmDiskUsageWMax             = 0.0
  , nmDiskUsageWMaxTotal        = 0.0
  , nmDiskUsageWPercent         = 0.0
  , nmDiskUsageWLast            = 0
  , nmDiskUsageWNs              = 10000
  , nmDiskUsageWAdaptTime       = UTCTime (ModifiedJulianDay 0) 0
  , nmDiskUsageWLastUpdate      = 0
  , nmNetworkUsageIn            = 0.0
  , nmNetworkUsageInPercent     = 0.0
  , nmNetworkUsageInMax         = 0.0
  , nmNetworkUsageInMaxTotal    = 0.0
  , nmNetworkUsageInLast        = 0
  , nmNetworkUsageInNs          = 10000
  , nmNetworkUsageInLastUpdate  = 0
  , nmNetworkUsageOut           = 0.0
  , nmNetworkUsageOutPercent    = 0.0
  , nmNetworkUsageOutMax        = 0.0
  , nmNetworkUsageOutMaxTotal   = 0.0
  , nmNetworkUsageOutLast       = 0
  , nmNetworkUsageOutNs         = 10000
  , nmNetworkUsageOutLastUpdate = 0
  , nmRTSMemoryAllocated        = 1.0
  , nmRTSMemoryUsed             = 0.1
  , nmRTSMemoryUsedPercent      = 1.0
  , nmRTSMemoryLastUpdate       = 0
  , nmRTSGcCpu                  = 0.1
  , nmRTSGcCpuLastUpdate        = 0
  , nmRTSGcElapsed              = 0.1
  , nmRTSGcElapsedLastUpdate    = 0
  , nmRTSGcNum                  = 0
  , nmRTSGcNumLastUpdate        = 0
  , nmRTSGcMajorNum             = 0
  , nmRTSGcMajorNumLastUpdate   = 0
  }
