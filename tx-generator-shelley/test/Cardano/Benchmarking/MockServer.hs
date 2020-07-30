{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Cardano.Benchmarking.MockServer
where
import           Control.Concurrent
import           Control.Concurrent.Async

import           Control.Concurrent.STM as STM
import           Control.Monad (forM_, void)
import qualified Control.Monad.Class.MonadSTM as MSTM
import           Control.Monad.IO.Class
import           Control.Tracer (Tracer, nullTracer)

import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Ledger.SupportsMempool as Mempool (GenTxId,txId)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx , mkShelleyTx)

import           Network.TypedProtocol.Proofs (connectPipelined)
import           Ouroboros.Network.Protocol.TxSubmission.Client (txSubmissionClientPeer)
import           Ouroboros.Network.Protocol.TxSubmission.Server
                    (TxSubmissionServerPipelined, txSubmissionServerPeerPipelined)
import           Ouroboros.Network.NodeToNode
                   (MiniProtocolParameters (..), defaultMiniProtocolParameters)

import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSubmission(..))
import           Network.TypedProtocol.Core (PeerRole(..))
import           Network.TypedProtocol.Pipelined (PeerPipelined)
import           Cardano.Benchmarking.TxGenerator.Submission
import           Cardano.Benchmarking.TxGenerator.Types as T

import           Cardano.Api.Typed as Api
import           Cardano.Slotting.Slot

import           Cardano.Benchmarking.ReferenceServer (txSubmissionServer)

-- an alternative path is possible via
-- Ouroboros.Network.Protocol.TxSubmission.Direct.directPipelined

testTxPeer :: forall blk. RunNode blk => [GenTx blk] -> IO [GenTx blk]
testTxPeer txs = do
  tmv <- MSTM.newEmptyTMVarM
  terminateEarly <- STM.newTVarIO False
  txInChan <- STM.newTMVarIO txs
  pipeline <- async $ mockPipeline nullTracer tmv
  generator <- async $ bulkSubmission updROEnv nullTracer terminateEarly txInChan tmv
  threadDelay $ 10 * 1000000
  atomically $ STM.writeTVar terminateEarly True
  wait pipeline
  where
    updROEnv :: ROEnv (Mempool.GenTxId blk) (GenTx blk) -> ROEnv (Mempool.GenTxId blk) (GenTx blk)
    updROEnv defaultROEnv =
        ROEnv { targetBacklog     = targetBacklog defaultROEnv
              , txNumServiceTime  = Just 0.1
              , txSizeServiceTime = Nothing
              }

mockPipeline
  :: RunNode block =>
     Tracer IO NodeToNodeSubmissionTrace
     -> STM.TMVar (RPCTxSubmission IO (GenTxId block) (GenTx block))
     -> IO [GenTx block]

mockPipeline tr3 tmv = do
  (receivedTxs, () , _terminalstate) <- connectPipelined []
             (txSubmissionServerPeerPipelined mockServer)
             (txSubmissionClientPeer (txSubmissionClient tr3 tmv))
  return receivedTxs

mockServer
  :: forall block. RunNode block
  =>  TxSubmissionServerPipelined
       (GenTxId block)
       (GenTx block)
       IO
       [GenTx block]
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

dummyTx :: Integer -> GenTx Block
dummyTx f = mkShelleyTx tx
  where
    (ShelleyTx tx) = Api.makeSignedTransaction [] $
      Api.makeShelleyTransaction
        Api.txExtraContentEmpty
        ( SlotNo 10000)
        ( Lovelace f )
        []
        []

testPipeline :: [ GenTx Block ] -> IO Bool
testPipeline sendTx = do
  receivedTx <- testTxPeer sendTx
  return $ receivedTx == sendTx

test = testPipeline $ map dummyTx [1..10]
