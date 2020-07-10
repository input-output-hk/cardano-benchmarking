{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Cardano.Benchmarking.Test
  ( testTxPeer
  )
where
import           Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import           Control.Monad (void)
import qualified Control.Monad.Class.MonadSTM as MSTM
import           Control.Monad.IO.Class
import           Control.Tracer (Tracer, nullTracer)

import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Ledger.SupportsMempool as Mempool (GenTxId,txId)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx)

import           Network.TypedProtocol.Proofs (connectPipelined)
import           Ouroboros.Network.Protocol.TxSubmission.Client (txSubmissionClientPeer)
import           Ouroboros.Network.Protocol.TxSubmission.Server
                    (TxSubmissionServerPipelined, txSubmissionServerPeerPipelined)
import           Ouroboros.Network.NodeToNode
                   (MiniProtocolParameters (..), defaultMiniProtocolParameters)
import           Ouroboros.Network.Protocol.TxSubmission.Examples (txSubmissionServer)

import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSubmission(..))
import           Network.TypedProtocol.Core (PeerRole(..))
import           Network.TypedProtocol.Pipelined (PeerPipelined)
import           Cardano.Benchmarking.TxGenerator.Submission
import           Cardano.Benchmarking.TxGenerator.Types as T


-- an alternative path is possible via
-- Ouroboros.Network.Protocol.TxSubmission.Direct.directPipelined

testTxPeer
  :: forall block txid tx.
     ( RunNode block
     , block ~ Block
     , tx ~ GenTx block
     )
  => [tx] -> IO [tx]
testTxPeer txs = do
  tmv <- MSTM.newEmptyTMVarM
  terminateEarly <- liftIO $ STM.newTVarIO False
  txInChan <- liftIO $ STM.newTMVarIO txs
  pipeline <- async $ mockPipeline nullTracer tmv
  generator <- async $ bulkSubmission updROEnv nullTracer terminateEarly txInChan tmv
  return []
  where
    updROEnv
        :: ROEnv (Mempool.GenTxId Block) (GenTx Block)
        -> ROEnv (Mempool.GenTxId Block) (GenTx Block)
    updROEnv defaultROEnv =
        ROEnv { targetBacklog     = targetBacklog defaultROEnv
              , txNumServiceTime  = Just 10 -- minimalTPSRate $ P.tps args
              , txSizeServiceTime = Nothing
              }


mockPipeline tr3 tmv
  = do
    (receivedTxs, () , _terminalstate) <- connectPipelined []
             (txSubmissionServerPeerPipelined mockServer)
             (txSubmissionClientPeer (txSubmissionClient tr3 tmv))
    return receivedTxs

mockServer
  :: TxSubmissionServerPipelined
       (GenTxId Block)
       (GenTx Block)
       IO
       [GenTx Block]
mockServer
  = txSubmissionServer
     nullTracer
     Mempool.txId
     (txSubmissionMaxUnacked defaultMiniProtocolParameters)
     maxTxIdsToRequest
     maxTxToRequest
  where
    maxTxIdsToRequest = 3
    maxTxToRequest    = 2
