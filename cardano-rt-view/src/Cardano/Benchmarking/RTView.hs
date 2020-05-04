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
import           Data.List
                   ( nub, nubBy )
import qualified System.Exit as Ex

import           Cardano.BM.Configuration
                   ( Configuration
                   , getAcceptAt, setup
                   )
import           Cardano.BM.Data.Configuration
                   ( RemoteAddrNamed (..), RemoteAddr (..) )
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
runCardanoRTView params = do
  config <- readConfig (rtvConfig params)
  acceptors <- checkIfTraceAcceptorIsDefined config (rtvConfig params)
  makeSureTraceAcceptorsAreUnique acceptors

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
  serverThr   <- async $ launchServer nodesStateMVar params acceptors

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
  -> IO [RemoteAddrNamed]
checkIfTraceAcceptorIsDefined config pathToConfig =
  getAcceptAt config >>= \case
    Just acceptors -> return acceptors
    Nothing -> Ex.die $ "No trace acceptors found in the configuration: " <> pathToConfig

-- | If configuration contains more than one trace acceptor,
--   check if they are unique, to avoid socket problems.
makeSureTraceAcceptorsAreUnique
  :: [RemoteAddrNamed]
  -> IO ()
makeSureTraceAcceptorsAreUnique acceptors = do
  checkIfNodesNamesAreUnique
  checkIfNetParametersAreUnique
 where
  checkIfNodesNamesAreUnique =
    when (length names /= length (nub names)) $
      Ex.die "Nodes' names in trace acceptors must be unique!"

  checkIfNetParametersAreUnique =
    when (length addrs /= length (nubBy compareNetParams addrs)) $
      Ex.die "Nodes' network parameters in trace acceptors must be unique!"

  compareNetParams (RemoteSocket h1 p1) (RemoteSocket h2 p2) = h1 == h2 && p1 == p2
  compareNetParams (RemoteSocket _ _)   (RemotePipe _)       = False
  compareNetParams (RemotePipe _)       (RemoteSocket _ _)   = False
  compareNetParams (RemotePipe p1)      (RemotePipe p2)      = p1 == p2

  names = [name | RemoteAddrNamed name _ <- acceptors]
  addrs = [addr | RemoteAddrNamed _ addr <- acceptors]
