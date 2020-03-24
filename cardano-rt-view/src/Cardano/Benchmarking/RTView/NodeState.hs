{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.NodeState
    ( NodeState (..)
    , NodeInfo (..)
    , NodeMetrics (..)
    , defaultNodeState
    ) where


import           Cardano.Prelude
import           Data.Time.Clock
                   ( UTCTime (..) )

data NodeState = NodeState
  { nsInfo    :: NodeInfo
  , nsMetrics :: NodeMetrics
  }

data NodeInfo = NodeInfo
  { niStartTime    :: UTCTime
  , niEpoch        :: Int
  , niSlot         :: Int
  , niBlocksNumber :: Int
  , niChainDensity :: Double
  , niTxsProcessed :: Int
  , niPort         :: Int
  , niPeersNumber  :: Int
  }

data NodeMetrics = NodeMetrics
  { nmMempoolKBMax      :: Double
  , nmMempoolKBPercent  :: Double
  , nmMempoolTxsMax     :: Int
  , nmMempoolTxsPercent :: Double
  , nmMemoryMax         :: Double
  , nmMemoryPercent     :: Double
  , nmCPUMax            :: Double
  , nmCPUPercent        :: Double
  , nmDiskReadMax       :: Double
  , nmDiskReadPercent   :: Double
  , nmDiskWriteMax      :: Double
  , nmDiskWritePercent  :: Double
  , nmNetworkInMax      :: Double
  , nmNetworkInPercent  :: Double
  , nmNetworkOutMax     :: Double
  , nmNetworkOutPercent :: Double
  }

defaultNodeState :: UTCTime -> NodeState
defaultNodeState now = NodeState
  { nsInfo    = defaultNodeInfo now
  , nsMetrics = defaultNodeMetrics
  }

defaultNodeInfo :: UTCTime -> NodeInfo
defaultNodeInfo now = NodeInfo
  { niStartTime    = now
  , niEpoch        = 0
  , niSlot         = 0
  , niBlocksNumber = 0
  , niChainDensity = 0.0
  , niTxsProcessed = 0
  , niPort         = 0
  , niPeersNumber  = 0
  }

defaultNodeMetrics :: NodeMetrics
defaultNodeMetrics = NodeMetrics
  { nmMempoolKBMax      = 0.0
  , nmMempoolKBPercent  = 0.0
  , nmMempoolTxsMax     = 0
  , nmMempoolTxsPercent = 0.0
  , nmMemoryMax         = 0.0
  , nmMemoryPercent     = 0.0
  , nmCPUMax            = 0.0
  , nmCPUPercent        = 0.0
  , nmDiskReadMax       = 0.0
  , nmDiskReadPercent   = 0.0
  , nmDiskWriteMax      = 0.0
  , nmDiskWritePercent  = 0.0
  , nmNetworkInMax      = 0.0
  , nmNetworkInPercent  = 0.0
  , nmNetworkOutMax     = 0.0
  , nmNetworkOutPercent = 0.0
  }
