{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView
    ( runCardanoRTView
    ) where

import           Cardano.Prelude hiding (newMVar)

import           Control.Concurrent.Async (async, waitAnyCancel)
import           Control.Concurrent.MVar.Strict (MVar, newMVar)
import qualified Data.Text.IO as TIO

import           Cardano.BM.Backend.Switchboard (addUserDefinedBackend)
import           Cardano.BM.Data.Backend (Backend (..))
import qualified Cardano.BM.Setup as Setup
import           Cardano.BM.Trace (Trace, logNotice)
import           Cardano.BM.Tracing (appendName)

import           Cardano.Benchmarking.RTView.Acceptor (launchMetricsAcceptor)
import           Cardano.Benchmarking.RTView.CLI (RTViewParams (..))
import           Cardano.Benchmarking.RTView.Config (prepareConfigAndParams)
import           Cardano.Benchmarking.RTView.ErrorBuffer (ErrorBuffer, effectuate, realize,
                                                          unrealize)
import           Cardano.Benchmarking.RTView.NodeState.Types (NodesState, defaultNodesState)
import           Cardano.Benchmarking.RTView.NodeState.Updater (launchNodeStateUpdater)
import           Cardano.Benchmarking.RTView.Server (launchServer)

-- | Run the service.
runCardanoRTView :: RTViewParams -> IO ()
runCardanoRTView params' = do
  TIO.putStrLn "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓"
  TIO.putStrLn "┃ RTView: real-time view service for cardano-node ┃"
  TIO.putStrLn "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"

  (config, params, acceptors) <- prepareConfigAndParams params'

  (tr :: Trace IO Text, switchBoard) <- Setup.setupTrace_ config "cardano-rt-view"
  let accTr = appendName "acceptor" tr

  -- Initialise own backend (error buffer).
  be :: ErrorBuffer Text <- realize config
  let ebBe = MkBackend { bEffectuate = effectuate be
                       , bUnrealize  = unrealize be
                       }
  addUserDefinedBackend switchBoard ebBe "ErrorBufferBK"

  logNotice tr "Starting service; hit CTRL-C to terminate..."

  initStateOfNodes <- defaultNodesState config
  -- This MVar contains state (info, metrics) for all nodes we receive metrics from.
  nodesStateMVar :: MVar NodesState <- newMVar initStateOfNodes

  -- Launch 3 threads:
  --   1. acceptor plugin (it launches |TraceAcceptor| plugin),
  --   2. node state updater (it gets metrics from |LogBuffer| and updates NodeState),
  --   3. server (it serves requests from user's browser and shows nodes' metrics in the real time).
  acceptorThr <- async $ launchMetricsAcceptor config accTr switchBoard
  updaterThr  <- async $ launchNodeStateUpdater tr switchBoard be nodesStateMVar
  serverThr   <- async $ launchServer nodesStateMVar params acceptors

  void $ waitAnyCancel [acceptorThr, updaterThr, serverThr]
