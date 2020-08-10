{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.MockServer
where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer, nullTracer)

import           Ouroboros.Consensus.Ledger.SupportsMempool as Mempool (GenTxId, txId)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx, mkShelleyTx)

import           Network.TypedProtocol.Proofs (connectPipelined)
import           Ouroboros.Network.NodeToNode (MiniProtocolParameters (..),
                                               defaultMiniProtocolParameters)
import           Ouroboros.Network.Protocol.TxSubmission.Client (txSubmissionClientPeer)
import           Ouroboros.Network.Protocol.TxSubmission.Server (TxSubmissionServerPipelined,
                                                                 txSubmissionServerPeerPipelined)

import           Cardano.Benchmarking.TxGenerator.Submission
import           Cardano.Benchmarking.TxGenerator.Types as T
import           Network.TypedProtocol.Core (Peer, PeerRole (..))
import           Network.TypedProtocol.Pipelined (PeerPipelined)
import           Network.TypedProtocol.Pipelined (PeerPipelined)
import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSubmission (..))

import           Cardano.Api.Typed as Api
import           Cardano.Slotting.Slot

import           Cardano.Benchmarking.ReferenceServer (txSubmissionServer)

-- An alternative path is possible via
-- Ouroboros.Network.Protocol.TxSubmission.Direct.directPipelined

testTxPeer
  :: forall m block. (RunNode block, MonadSTM m , MonadAsync m, MonadTime m, MonadDelay m)
  =>  [GenTx block] -> m [GenTx block]
testTxPeer txs = do
  tmv <- newEmptyTMVarM
  terminateEarly <- newTVarM False
  txInChan <- newTMVarM txs
  pipeline <- async $ mockPipeline nullTracer tmv
  _generator <- async $ bulkSubmission updROEnv nullTracer terminateEarly txInChan tmv
  threadDelay $ 10 * 1000000
  atomically $ writeTVar terminateEarly True
  wait pipeline
  where
    updROEnv :: ROEnv (Mempool.GenTxId blk) (GenTx blk) -> ROEnv (Mempool.GenTxId blk) (GenTx blk)
    updROEnv defaultROEnv =
        ROEnv { targetBacklog     = targetBacklog defaultROEnv
              , txNumServiceTime  = Just 0.1
              , txSizeServiceTime = Nothing
              }

mockPipeline
  :: forall m block. (RunNode block, MonadSTM m ) =>
     Tracer m NodeToNodeSubmissionTrace
     -> TMVar m (RPCTxSubmission m (GenTxId block) (GenTx block))
     -> m [GenTx block]
mockPipeline tr3 tmv = do
  (receivedTxs, () , _terminalstate) <- connectPipelined [] server client
  return receivedTxs
  where
    client ::
        Peer
          (TxSubmission (GenTxId block) (GenTx block))
         'AsClient
         'StIdle
         m
         ()
    client = txSubmissionClientPeer (txSubmissionClient tr3 tmv)
    server ::
        PeerPipelined
          (TxSubmission (GenTxId block) (GenTx block))
          'AsServer
          'StIdle
          m
          [GenTx block]
    server = txSubmissionServerPeerPipelined mockServer

mockServer
  :: forall m block. (RunNode block, Monad m)
  =>  TxSubmissionServerPipelined
       (GenTxId block)
       (GenTx block)
       m
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

testPipeline :: [ GenTx Block ] -> Bool
testPipeline sendTx
  = (runSimOrThrow $ testTxPeer sendTx) == sendTx

test :: Bool
test = testPipeline $ map dummyTx [1..10]
