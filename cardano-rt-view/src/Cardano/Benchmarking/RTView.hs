{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView
    ( runCardanoRTView
    ) where

import           Cardano.Prelude

import           Control.Concurrent.Async
                   ( async, waitAnyCancel )
import           Control.Concurrent.MVar
                   ( MVar
                   , newMVar
                   )
import qualified System.Exit as Ex

import           Cardano.BM.Configuration
                   ( Configuration
                   , getAcceptAt, setup
                   )
import qualified Cardano.BM.Setup as Setup
import           Cardano.BM.Trace
                   ( Trace
                   , logNotice
                   )
import           Cardano.BM.Tracing
                   ( appendName )

import           Cardano.Benchmarking.RTView.CLI
                   ( RTViewParams (..) )
import           Cardano.Benchmarking.RTView.Acceptor
                   ( launchMetricsAcceptor )
import           Cardano.Benchmarking.RTView.NodeState.Types
                   ( NodesState
                   , defaultNodesState
                   )
import           Cardano.Benchmarking.RTView.NodeState.Updater
                   ( launchNodeStateUpdater )
import           Cardano.Benchmarking.RTView.Server
                   ( launchServer )

-- | Run the service.
runCardanoRTView :: RTViewParams -> IO ()
runCardanoRTView (RTViewParams pathToConfig pathToStatic port) = do
  config <- readConfig pathToConfig
  checkIfTraceAcceptorIsDefined config pathToConfig

  (tr :: Trace IO Text, switchBoard) <- Setup.setupTrace_ config "cardano-rt-view"
  let accTr = appendName "acceptor" tr

  logNotice tr "Starting service; hit CTRL-C to terminate..."

  initStateOfNodes <- defaultNodesState config
  -- This MVar contains state (info, metrics) for all nodes we receive metrics from.
  nodesStateMVar :: MVar NodesState <- newMVar initStateOfNodes

  -- Launch 3 threads:
  --   1. acceptor plugin (it launches |TraceAcceptor| plugin),
  --   2. node state updater (it gets metrics from |LogBuffer| and updates NodeState),
  --   3. server (it serves requests from user's browser and shows nodes' metrics in the real time).
  acceptorThr <- async $ launchMetricsAcceptor config accTr switchBoard
  updaterThr  <- async $ launchNodeStateUpdater tr switchBoard nodesStateMVar
  serverThr   <- async $ launchServer nodesStateMVar pathToStatic port

  void $ waitAnyCancel [acceptorThr, updaterThr, serverThr]

-- | Reads the service' configuration file (path is passed via '--config' CLI option).
readConfig :: FilePath -> IO Configuration
readConfig pathToConfig = setup pathToConfig `catch` exceptHandler
 where
  exceptHandler :: IOException -> IO Configuration
  exceptHandler e =
    Ex.die $ "Exception while reading configuration "
             <> pathToConfig
             <> ", exception: "
             <> show e

-- | RTView service requires at least one |TraceAcceptor|.
checkIfTraceAcceptorIsDefined
  :: Configuration
  -> FilePath
  -> IO ()
checkIfTraceAcceptorIsDefined config pathToConfig =
  getAcceptAt config >>= \case
    Just _  -> return ()
    Nothing -> Ex.die $ "No trace acceptors found in the configuration: " <> pathToConfig
