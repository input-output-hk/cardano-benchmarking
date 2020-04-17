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
import qualified Data.Map.Strict as Map
import           Data.Map.Strict
                   ( Map )
import           Data.Time.Clock
                   ( UTCTime (..)
                   , getCurrentTime
                   )

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
  } deriving Show

data PeerInfo = PeerInfo
  { piEndpoint    :: !String
  , piSlotNumber  :: !String
  , piBlockNumber :: !String
  } deriving (Eq, Show)

data NodeError = NodeError
  { eTimestamp :: !UTCTime
  , eSeverity  :: !Severity
  , eMessage   :: !String
  } deriving Show

data NodeInfo = NodeInfo
  { niNodeRelease       :: !String
  , niNodeVersion       :: !String
  , niNodeCommit        :: !String
  , niNodeShortCommit   :: !String
  , niStartTime         :: !UTCTime
  , niEpoch             :: !Integer
  , niSlot              :: !Integer
  , niBlocksNumber      :: !Integer
  , niChainDensity      :: !Double
  , niTxsProcessed      :: !Integer
  , niPeersNumber       :: !Integer
  , niPeersInfo         :: ![PeerInfo]
  , niTraceAcceptorHost :: !String
  , niTraceAcceptorPort :: !String
  , niNodeErrors        :: ![NodeError]
  } deriving Show

data NodeMetrics = NodeMetrics
  { nmMempoolTxsNumber        :: !Word64
  , nmMempoolTxsPercent       :: !Double
  , nmMempoolBytes            :: !Word64
  , nmMempoolBytesPercent     :: !Double
  , nmMempoolCapacity         :: !Word64
  , nmMempoolCapacityBytes    :: !Word64
  , nmMemory                  :: !Double
  , nmMemoryMax               :: !Double
  , nmMemoryMaxTotal          :: !Double
  , nmMemoryPercent           :: !Double
  , nmCPUPercent              :: !Double
  , nmCPULast                 :: !Integer
  , nmCPUNs                   :: !Word64
  , nmDiskUsageR              :: !Double
  , nmDiskUsageRMax           :: !Double
  , nmDiskUsageRMaxTotal      :: !Double
  , nmDiskUsageRPercent       :: !Double
  , nmDiskUsageRLast          :: !Word64
  , nmDiskUsageRNs            :: !Word64
  , nmDiskUsageW              :: !Double
  , nmDiskUsageWMax           :: !Double
  , nmDiskUsageWMaxTotal      :: !Double
  , nmDiskUsageWPercent       :: !Double
  , nmDiskUsageWLast          :: !Word64
  , nmDiskUsageWNs            :: !Word64
  , nmNetworkUsageIn          :: !Double
  , nmNetworkUsageInPercent   :: !Double
  , nmNetworkUsageInMax       :: !Double
  , nmNetworkUsageInMaxTotal  :: !Double
  , nmNetworkUsageInLast      :: !Word64
  , nmNetworkUsageInNs        :: !Word64
  , nmNetworkUsageOut         :: !Double
  , nmNetworkUsageOutPercent  :: !Double
  , nmNetworkUsageOutMax      :: !Double
  , nmNetworkUsageOutMaxTotal :: !Double
  , nmNetworkUsageOutLast     :: !Word64
  , nmNetworkUsageOutNs       :: !Word64
  , nmRTSMemoryAllocated      :: !Double
  , nmRTSMemoryUsed           :: !Double
  , nmRTSMemoryUsedPercent    :: !Double
  , nmRTSGcCpu                :: !Double
  , nmRTSGcElapsed            :: !Double
  , nmRTSGcNum                :: !Integer
  , nmRTSGcMajorNum           :: !Integer
  } deriving Show

defaultNodesState
  :: Configuration
  -> IO NodesState
defaultNodesState config =
  CM.getAcceptAt config >>= \case
    Just remoteAddresses -> do
      now <- getCurrentTime
      return $ Map.fromList [(name, defaultNodeState now) | (RemoteAddrNamed name _) <- remoteAddresses]
    Nothing ->
      -- Actually it's impossible, because at this point we already know
      -- that at least one |TraceAcceptor| is defined in the config.
      return Map.empty

defaultNodeState
  :: UTCTime
  -> NodeState
defaultNodeState now = NodeState
  { nsInfo    = defaultNodeInfo now
  , nsMetrics = defaultNodeMetrics
  }

defaultNodeInfo
  :: UTCTime
  -> NodeInfo
defaultNodeInfo now = NodeInfo
  { niNodeRelease       = "-"
  , niNodeVersion       = "-"
  , niNodeCommit        = "-"
  , niNodeShortCommit   = "-"
  , niStartTime         = now
  , niEpoch             = 0
  , niSlot              = 0
  , niBlocksNumber      = 0
  , niChainDensity      = 0.0
  , niTxsProcessed      = 0
  , niPeersNumber       = 0
  , niPeersInfo         = []
  , niTraceAcceptorHost = "-"
  , niTraceAcceptorPort = "-"
  , niNodeErrors        = []
  }

defaultNodeMetrics :: NodeMetrics
defaultNodeMetrics = NodeMetrics
  { nmMempoolTxsNumber        = 0
  , nmMempoolTxsPercent       = 0.0
  , nmMempoolBytes            = 0
  , nmMempoolBytesPercent     = 0.0
  , nmMempoolCapacity         = 200
  , nmMempoolCapacityBytes    = 200 * maxBytesPerTx
  , nmMemory                  = 0.0
  , nmMemoryMax               = 0.0
  , nmMemoryMaxTotal          = 200.0
  , nmMemoryPercent           = 0.0
  , nmCPUPercent              = 0.5
  , nmCPULast                 = 0
  , nmCPUNs                   = 10000
  , nmDiskUsageR              = 0.0
  , nmDiskUsageRMax           = 0.0
  , nmDiskUsageRMaxTotal      = 0.0
  , nmDiskUsageRPercent       = 0.0
  , nmDiskUsageRLast          = 0
  , nmDiskUsageRNs            = 10000
  , nmDiskUsageW              = 0.0
  , nmDiskUsageWMax           = 0.0
  , nmDiskUsageWMaxTotal      = 0.0
  , nmDiskUsageWPercent       = 0.0
  , nmDiskUsageWLast          = 0
  , nmDiskUsageWNs            = 10000
  , nmNetworkUsageIn          = 0.0
  , nmNetworkUsageInPercent   = 0.0
  , nmNetworkUsageInMax       = 0.0
  , nmNetworkUsageInMaxTotal  = 0.0
  , nmNetworkUsageInLast      = 0
  , nmNetworkUsageInNs        = 10000
  , nmNetworkUsageOut         = 0.0
  , nmNetworkUsageOutPercent  = 0.0
  , nmNetworkUsageOutMax      = 0.0
  , nmNetworkUsageOutMaxTotal = 0.0
  , nmNetworkUsageOutLast     = 0
  , nmNetworkUsageOutNs       = 10000
  , nmRTSMemoryAllocated      = 1.0
  , nmRTSMemoryUsed           = 0.1
  , nmRTSMemoryUsedPercent    = 1.0
  , nmRTSGcCpu                = 0.1
  , nmRTSGcElapsed            = 0.1
  , nmRTSGcNum                = 0
  , nmRTSGcMajorNum           = 0
  }
 where
  maxBytesPerTx = 4096 :: Word64
