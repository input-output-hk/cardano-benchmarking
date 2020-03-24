{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView
    ( runCardanoRTView
    ) where

import           Cardano.Prelude

import           Control.Concurrent.Async
                   ( async, waitBoth )
import           Control.Concurrent.MVar
                   ( MVar
                   , newMVar
                   )
import           Data.Time.Clock
                   ( getCurrentTime )
import qualified System.Exit as Ex

import           Cardano.BM.Backend.LogBuffer
                   ( LogBuffer )
import           Cardano.BM.Backend.Switchboard
                   ( realize )
import           Cardano.BM.Configuration
                   ( Configuration
                   , getAcceptAt, setup
                   )
import qualified Cardano.BM.Setup as Setup
import           Cardano.BM.Trace
                   ( Trace )
import           Cardano.BM.Tracing
                   ( appendName )

import           Cardano.Benchmarking.RTView.CLI
                   ( RTViewParams (..) )
import           Cardano.Benchmarking.RTView.Acceptor
                   ( {-launchMetricsAcceptor,-} launchFakeAcceptor )
import           Cardano.Benchmarking.RTView.NodeState
                   ( NodeState (..)
                   , defaultNodeState
                   )
import           Cardano.Benchmarking.RTView.Server
                   ( launchServer )


-- | Run the service.
runCardanoRTView :: RTViewParams -> IO ()
runCardanoRTView (RTViewParams pathToConfig pathToStatic port) = do
  config <- readConfig pathToConfig
  checkIfTraceAcceptorIsDefined config pathToConfig

  (tr :: Trace IO Text, _switchBoard) <- Setup.setupTrace_ config "cardano-rt-view"
  let _accTr = appendName "acceptor" tr

  -- All |LogObject|s received by |TraceAcceptor|s will be redirected to |LogBuffer|.
  logBufferBE :: LogBuffer Text <- realize config

  now <- getCurrentTime
  -- This MVar contains node state (info, metrics).
  nodeStateMVar :: MVar NodeState <- newMVar (defaultNodeState now)

  -- Launch 3 threads:
  --   1. acceptor plugin (it runs |TraceAcceptor| plugin),
  --   2. node state updater (it get metrics from |LogBuffer| and store them in nodeStateMVar),
  --   3. server (it serves requests from user's browser and shows these metrics in real time).

  -- acceptorThr <- async $ launchMetricsAcceptor config accTr be switchBoard
  acceptorThr <- async $ launchFakeAcceptor logBufferBE nodeStateMVar
  serverThr <- async $ launchServer nodeStateMVar pathToStatic port

  void $ waitBoth acceptorThr serverThr

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
