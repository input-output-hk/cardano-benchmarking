{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.TxGenerator
  ( TxGenError
  , genesisBenchmarkRunner
  )
where
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import           Control.Monad (forM, mapM, when, void)
import qualified Control.Monad.Class.MonadSTM as MSTM
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import qualified Data.List.NonEmpty as NE
import           Network.Socket (AddrInfo)

import           Cardano.Node.Logging (LoggingLayer (..))

import           Cardano.Benchmarking.TxGenerator.Error (TxGenError (..))
import           Cardano.Benchmarking.TxGenerator.NodeToNode (BenchmarkTxSubmitTracers (..),
                     benchmarkConnectTxSubmit)

import           Control.Tracer (Tracer, traceWith)

import           Ouroboros.Consensus.Shelley.Node as Ouroboros (protocolClientInfoShelley)
import           Ouroboros.Network.NodeToClient (IOManager)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Ledger.SupportsMempool as Mempool (GenTxId)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx, mkShelleyTx)
import           Cardano.Api.Typed as Api

import qualified Cardano.Benchmarking.TxGenerator.CLI.Parsers as P
import           Cardano.Benchmarking.TxGenerator.Producer as Producer
import           Cardano.Benchmarking.TxGenerator.Phase1
import           Cardano.Benchmarking.TxGenerator.Submission
import           Cardano.Benchmarking.TxGenerator.Types as T
import           Cardano.Benchmarking.TxGenerator.Utils

-----------------------------------------------------------------------------------------
-- | Genesis benchmark runner (we call it in 'Run.runNode').
--
--   Using a _richman_ (from genesis block) to supply some initial
--   amount of funds for disbursment.
-----------------------------------------------------------------------------------------
genesisBenchmarkRunner
  :: P.GenerateTxs
  -> LoggingLayer
  -> IOManager
  -> ExceptT TxGenError IO ()
genesisBenchmarkRunner args loggingLayer iocp = do
  do
    let tps = unTPSRate $ P.tps args
    when (tps < 0.05) $ left $ TooSmallTPSRate $ tps

  let ( benchTracer , connectTracer , submitMuxTracer , _lowLevelSubmitTracer , submitTracer )
          = createTracers loggingLayer

  let traceIO = traceWith benchTracer . TraceBenchTxSubDebug
      trace = liftIO . traceIO
  trace "******* Tx generator, tracers are ready *******"

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, signing keys are ready *******"

  trace "******* Tx generator, phase 1: make enough available UTxO entries *******"

  remoteAddresses <- liftIO $ mapM resolveRemoteAddr $ P.nodeAdresses args

  producers <- runPhase1 args remoteAddresses

  do let coolDown = unInitCoolDown $ P.coolDownDelay args
     trace $ "******* Tx generator: waiting " ++ show coolDown ++ "s *******"
     liftIO $ threadDelay (coolDown * 1000 * 1000)

  trace "******* Tx generator, phase 2: pay to recipients *******"
  let benchmarkTracers :: BenchmarkTxSubmitTracers IO Block
      benchmarkTracers = BenchmarkTracers
                           { trSendRecvConnect      = connectTracer
                           , trSendRecvTxSubmission = submitMuxTracer
                           }


  let updROEnv
        :: ROEnv (Mempool.GenTxId Block) (GenTx Block)
        -> ROEnv (Mempool.GenTxId Block) (GenTx Block)
      updROEnv defaultROEnv =
        ROEnv { targetBacklog     = targetBacklog defaultROEnv
              , txNumServiceTime  = Just $ minimalTPSRate $ P.tps args
              , txSizeServiceTime = Nothing
              }

  let
    mkNodeTxList addr p = (addr, [ mkShelleyTx x | ShelleyTx x <- txs ])
      where
        txs = case payWithChangeSeq (replicate (unNumberOfTxs $ P.txCount args) $ Lovelace 111) p of
                (Right (x,_,_)) -> x
                (Left _err) -> error "unsufficient funds: mkNodeTxList"
    targetNodesAddrsAndTxsLists = zipWith mkNodeTxList (NE.toList remoteAddresses) producers

  txSubmissionTerm <- liftIO $ STM.newTVarIO False
  case P.explorerAPI args of
    Nothing ->
      -- There's no Explorer's API endpoint specified, submit transactions
      -- to the target nodes via 'ouroboros-network'.
      liftIO $ do
        traceIO $ "******* Tx generator, launching Tx peers:  "
                ++ show (length targetNodesAddrsAndTxsLists) ++ " of them"
        allAsyncs <- forM targetNodesAddrsAndTxsLists $ \(remoteAddr, txsList) -> do
          txsListGeneral :: STM.TMVar [GenTx Block] <- liftIO $ STM.newTMVarIO txsList

          r <- launchTxPeer benchTracer
                       benchmarkTracers
                       submitTracer
                       iocp
                       (P.network args)
                       txSubmissionTerm
                       Nothing
                       remoteAddr
                       updROEnv
                       txsListGeneral
          traceIO $ "******* Tx generator, launched a submission peer for " <> show remoteAddr
          pure r
        traceIO $ "******* Tx generator, all "
                  ++ show (length targetNodesAddrsAndTxsLists) ++ " peers started"
        let allAsyncs' = concat [[c, s] | (c, s) <- allAsyncs]
        -- Just wait for all threads to complete.
        mapM_ (void . wait) allAsyncs'
    Just (ExplorerAPIEnpoint _endpoint) ->
      -- Explorer's API endpoint is specified, submit transactions
      -- to that endpoint using POST-request.
{-      liftIO $ do
        initialRequest <- parseRequest endpoint
        txsList <- concat <$> mapM (STM.atomically . STM.takeTMVar) txsLists
        submitTxsToExplorer benchTracer initialRequest txsList tpsRate"
-}
      error "Just (ExplorerAPIEnpoint endpoint)"

---------------------------------------------------------------------------------------------------
-- Txs for submission.
---------------------------------------------------------------------------------------------------

-- | To get higher performance we need to hide latency of getting and
-- forwarding (in sufficient numbers) transactions.
--
-- TODO: transform comments into haddocks.
--
launchTxPeer
  :: forall block txid tx.
     ( RunNode block
     , txid ~ Mempool.GenTxId block
     , tx ~ GenTx block
     , block ~ Block
     )
  => Tracer IO (TraceBenchTxSubmit txid)
  -- Tracer carrying the benchmarking events
  -> BenchmarkTxSubmitTracers IO block
  -- tracer for lower level connection and details of
  -- protocol interactisn, intended for debugging
  -- associated issues.
  -> Tracer IO NodeToNodeSubmissionTrace
  -> IOManager
  -- ^ associate a file descriptor with IO completion port
  -> NetworkId
  -- ^ network magic
  -> MSTM.TVar IO Bool
  -- a "global" stop variable, set to True to force shutdown
  -> Maybe Network.Socket.AddrInfo
  -- local address binding (if wanted)
  -> Network.Socket.AddrInfo
  -- Remote address
  -> (ROEnv txid tx -> ROEnv txid tx)
  -- modifications to the submission engine enviroment to
  -- control rate etc
  -> MSTM.TMVar IO [tx]
  -- give this peer 1 or more transactions, empty list
  -- signifies stop this peer
  -> IO (Async (), Async ())
launchTxPeer tr1 tr2 tr3 iocp network termTM localAddr remoteAddr updROEnv txInChan = do
  tmv <- MSTM.newEmptyTMVarM
  (,) <$> (async $ benchmarkConnectTxSubmit
                     iocp
                     tr2
                     Ouroboros.protocolClientInfoShelley
                     network
                     localAddr
                     remoteAddr
                     (txSubmissionClient tr3 tmv))
      <*> (async $ bulkSubmission updROEnv tr1 termTM txInChan tmv)
