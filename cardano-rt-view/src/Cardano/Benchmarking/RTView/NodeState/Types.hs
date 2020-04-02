{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.NodeState.Types
    ( NodesState
    , NodeState (..)
    , NodeInfo (..)
    , NodeMetrics (..)
    , defaultNodesState
    ) where

import           Cardano.Prelude
import           Prelude
                   ( String, read )
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
import           Cardano.BM.Data.Configuration
                   ( RemoteAddrNamed (..), RemoteAddr (..) )

type NodesState = Map Text NodeState

data NodeState = NodeState
  { nsInfo    :: !NodeInfo
  , nsMetrics :: !NodeMetrics
  }

data NodeInfo = NodeInfo
  { niStartTime         :: !UTCTime
  , niEpoch             :: !Int
  , niSlot              :: !Int
  , niBlocksNumber      :: !Int
  , niChainDensity      :: !Double
  , niTxsProcessed      :: !Int
  , niPeersNumber       :: !Int
  , niTraceAcceptorHost :: !String
  , niTraceAcceptorPort :: !Int
  }

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
  }

defaultNodesState
  :: Configuration
  -> IO NodesState
defaultNodesState config =
  CM.getAcceptAt config >>= \case
    Just remoteAddresses -> do
      now <- getCurrentTime
      defaultStates <-
        forM remoteAddresses $ \(RemoteAddrNamed name addr) ->
          case addr of
            RemoteSocket host port ->
              return (name, defaultNodeState now host port)
            RemotePipe _ ->
              return (name, defaultNodeState now "none" "3000")
      return $ Map.fromList defaultStates
    Nothing ->
      -- Actually it's impossible, because at this point
      -- we already know that at least one |TraceAcceptor|
      -- is defined in the config.
      return Map.empty

defaultNodeState
  :: UTCTime
  -> String
  -> String
  -> NodeState
defaultNodeState now host port = NodeState
  { nsInfo    = defaultNodeInfo now host port
  , nsMetrics = defaultNodeMetrics
  }

defaultNodeInfo
  :: UTCTime
  -> String
  -> String
  -> NodeInfo
defaultNodeInfo now host port = NodeInfo
  { niStartTime         = now
  , niEpoch             = 0
  , niSlot              = 0
  , niBlocksNumber      = 0
  , niChainDensity      = 0.0
  , niTxsProcessed      = 0
  , niPeersNumber       = 0
  , niTraceAcceptorHost = host
  , niTraceAcceptorPort = read port :: Int
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
  }
 where
  maxBytesPerTx = 4096 :: Word64
