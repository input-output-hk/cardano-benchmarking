{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Benchmarking.TxGenerator.Types
where

import           Control.Monad.IO.Class
import           Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as A
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time.Clock (DiffTime, getCurrentTime)
import           Data.Word

import           Cardano.BM.Tracing
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Tracer (emptyObject, mkObject, trStructured)

import           Cardano.TracingOrphanInstances.Byron ()
import           Cardano.TracingOrphanInstances.Common ()
import           Cardano.TracingOrphanInstances.Consensus ()
import           Cardano.TracingOrphanInstances.Mock ()
import           Cardano.TracingOrphanInstances.Shelley ()
import           Cardano.TracingOrphanInstances.Network ()

import qualified Cardano.Chain.UTxO as CC.UTxO

import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Ouroboros.Consensus.Byron.Ledger.Mempool as Mempool (GenTx)
import           Ouroboros.Consensus.Ledger.SupportsMempool as Mempool
                   ( GenTxId, TxId)
import           Ouroboros.Network.Driver (TraceSendRecv (..))
import qualified Ouroboros.Network.Protocol.TxSubmission.Type as TS

import           Control.Monad.Class.MonadSTM (TMVar)

type              Block = ShelleyBlock  TPraosStandardCrypto
type              Transaction = CC.UTxO.ATxAux ByteString

newtype NumberOfTxs = NumberOfTxs {unNumberOfTxs :: Int}
  deriving (Eq, Ord, Show)

newtype NumberOfInputsPerTx =
  NumberOfInputsPerTx Int
  deriving (Eq, Ord, Show)

newtype NumberOfOutputsPerTx =
  NumberOfOutputsPerTx Int
  deriving (Eq, Ord, Show)

newtype FeePerTx = FeePerTx {unFeePerTx :: Word64}
  deriving (Eq, Ord, Show)

newtype TPSRate = TPSRate {unTPSRate :: Float}
  deriving (Eq, Ord, Show)

-- | How long wait before starting the main submission phase,
--   after the init Tx batch was submitted.
newtype InitCoolDown = InitCoolDown {unInitCoolDown :: Int}
  deriving (Eq, Ord, Show)

-- | This parameter specifies additional size (in bytes) of transaction.
--   Since 1 transaction is ([input] + [output] + attributes), its size
--   is defined by its inputs and outputs. We want to have an ability to
--   increase transaction's size without increasing the number of inputs/
--   outputs. Such a big transaction will give us more real-world results
--   of benchmarking.
--   Technically this parameter specifies the size of attribute we'll
--   add to transaction (by default attributes are empty, so if this
--   parameter is skipped, attributes will remain empty).
newtype TxAdditionalSize =
  TxAdditionalSize Int
  deriving (Eq, Ord, Show)

-- | This parameter specifies Explorer's API endpoint we use to submit
--   transaction. This parameter is an optional one, and if it's defined -
--   generator won't submit transactions to 'ouroboros-network', instead it
--   will submit transactions to that endpoint, using POST-request.
newtype ExplorerAPIEnpoint =
  ExplorerAPIEnpoint String
  deriving (Eq, Ord, Show)

-- | Tracer
data TraceBenchTxSubmit txid
  = TraceBenchTxSubRecv [txid]
  -- ^ Received from generator.
  | TraceBenchTxSubStart [txid]
  -- ^ The @txid@ has been submitted to `TxSubmission`
  --   protocol peer.
  | TraceBenchTxSubServAnn [txid]
  -- ^ Announcing txids in response for server's request.
  | TraceBenchTxSubServReq [txid]
  -- ^ Request for @tx@ recieved from `TxSubmission` protocol
  --   peer.
  | TraceBenchTxSubServAck [txid]
  -- ^ An ack (window moved over) received for these transactions.
  | TraceBenchTxSubServDrop [txid]
  -- ^ Transactions the server implicitly dropped.
  | TraceBenchTxSubServOuts [txid]
  -- ^ Transactions outstanding.
  | TraceBenchTxSubIdle
  -- ^ Remote peer requested new transasctions but none were
  --   available, generator not keeping up?
  | TraceBenchTxSubRateLimit DiffTime
  -- ^ Rate limiter bit, this much delay inserted to keep within
  --   configured rate.
  | TraceBenchTxSubDebug String
  | TraceBenchTxSubError Text
  deriving (Show)

-- | RPC interaction with `TxSubmission`
data RPCTxSubmission m txid tx
  = RPCRequestTxIdsPromptly (Word16, Word16) (TMVar m [(txid, TS.TxSizeInBytes)])
  -- ^ Request contains the acknowledged number and the size of the
  --   open window. Response contains the list of transactions (that
  --   can be empty - see the `TxSubmission` description of
  --   `StBlockingStyle` for more details). A prompt response
  --   is expected.
  |  RPCRequestTxIds (Word16, Word16) (TMVar m (Maybe [(txid, TS.TxSizeInBytes)]))
  -- ^ Request contains the acknowledged number and the size of the
  --   open window. Response contains the list of transactions (that
  --   can not be empty - see the `TxSubmission` description
  --   of `StBlockingStyle` for more details); `Nothing`
  --   indicates no more transaction submissions and a clean
  --   shutdown. A prompt response is not expected.
  | RPCRequestTxs [txid] (TMVar m [tx])
  -- ^ Request contains the list of transaction identifiers which are
  --   returned in the response.

-- | Low-tevel tracer
data TraceLowLevelSubmit
  = TraceLowLevelSubmitting
  -- ^ Submitting transaction.
  | TraceLowLevelAccepted
  -- ^ The transaction has been accepted.
  | TraceLowLevelRejected String
  -- ^ The transaction has been rejected, with corresponding error message.
  deriving (Show)

data NodeToNodeSubmissionTrace
  = ReqIdsBlocking  Word16 Word16
  | IdsListBlocking Int

  | ReqIdsPrompt    Word16 Word16
  | IdsListPrompt   Int

  | ReqTxs          Int
  | TxList          Int

  | EndOfProtocol

instance ToObject NodeToNodeSubmissionTrace where
  toObject MinimalVerbosity = const emptyObject -- do not log
  toObject _ = \case
    ReqIdsBlocking  ack req -> mkObject [ "kind" .= A.String "ReqIdsBlocking"
                                        , "ack"  .= A.toJSON ack
                                        , "req"  .= A.toJSON req ]
    IdsListBlocking sent    -> mkObject [ "kind" .= A.String "IdsListBlocking"
                                        , "sent" .= A.toJSON sent ]
    ReqIdsPrompt    ack req -> mkObject [ "kind" .= A.String "ReqIdsPrompt"
                                        , "ack"  .= A.toJSON ack
                                        , "req"  .= A.toJSON req ]
    IdsListPrompt   sent    -> mkObject [ "kind" .= A.String "IdsListPrompt"
                                        , "sent" .= A.toJSON sent ]
    EndOfProtocol           -> mkObject [ "kind" .= A.String "EndOfProtocol" ]
    ReqTxs          req     -> mkObject [ "kind" .= A.String "ReqTxs"
                                        , "req"  .= A.toJSON req ]
    TxList          sent    -> mkObject [ "kind" .= A.String "TxList"
                                        , "sent" .= A.toJSON sent ]

instance HasSeverityAnnotation NodeToNodeSubmissionTrace
instance HasPrivacyAnnotation  NodeToNodeSubmissionTrace
instance Transformable Text IO NodeToNodeSubmissionTrace where
  trTransformer = trStructured

{-instance {-# OVERLAPS #-} Show (GenTxId blk)
 => ToJSON (Mempool.GenTxId blk) where
  toJSON txid = A.String (T.pack $ show txid)
-}

instance ToObject (TraceBenchTxSubmit (Mempool.GenTxId Block)) where
  toObject MinimalVerbosity _ = emptyObject -- do not log
  toObject NormalVerbosity t =
    case t of
      TraceBenchTxSubRecv _      -> mkObject ["kind" .= A.String "TraceBenchTxSubRecv"]
      TraceBenchTxSubStart _     -> mkObject ["kind" .= A.String "TraceBenchTxSubStart"]
      TraceBenchTxSubServAnn _   -> mkObject ["kind" .= A.String "TraceBenchTxSubServAnn"]
      TraceBenchTxSubServReq _   -> mkObject ["kind" .= A.String "TraceBenchTxSubServReq"]
      TraceBenchTxSubServAck _   -> mkObject ["kind" .= A.String "TraceBenchTxSubServAck"]
      TraceBenchTxSubServDrop _  -> mkObject ["kind" .= A.String "TraceBenchTxSubServDrop"]
      TraceBenchTxSubServOuts _  -> mkObject ["kind" .= A.String "TraceBenchTxSubServOuts"]
      TraceBenchTxSubIdle        -> mkObject ["kind" .= A.String "TraceBenchTxSubIdle"]
      TraceBenchTxSubRateLimit _ -> mkObject ["kind" .= A.String "TraceBenchTxSubRateLimit"]
      TraceBenchTxSubDebug _     -> mkObject ["kind" .= A.String "TraceBenchTxSubDebug"]
      TraceBenchTxSubError _     -> mkObject ["kind" .= A.String "TraceBenchTxSubError"]
  toObject MaximalVerbosity t =
    case t of
      TraceBenchTxSubRecv txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubRecv"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubStart txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubStart"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServAnn txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServAnn"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServReq txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServReq"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServAck txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServAck"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServDrop txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServDrop"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServOuts txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServOuts"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubIdle ->
        mkObject [ "kind" .= A.String "TraceBenchTxSubIdle"
                 ]
      TraceBenchTxSubRateLimit limit ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubRateLimit"
                 , "limit" .= toJSON limit
                 ]
      TraceBenchTxSubDebug s ->
        mkObject [ "kind" .= A.String "TraceBenchTxSubDebug"
                 , "msg"  .= A.String (T.pack s)
                 ]
      TraceBenchTxSubError s ->
        mkObject [ "kind" .= A.String "TraceBenchTxSubError"
                 , "msg"  .= A.String s
                 ]

instance ToObject (Mempool.GenTxId Block) where
  toObject MinimalVerbosity _    = emptyObject -- do not log
  toObject NormalVerbosity _     = mkObject [ "kind" .= A.String "GenTxId"]
  toObject MaximalVerbosity txid = mkObject [ "kind" .= A.String "GenTxId"
                                            , "txId" .= toJSON txid
                                            ]

instance HasSeverityAnnotation (Mempool.GenTxId Block)

instance HasPrivacyAnnotation (Mempool.GenTxId Block)

instance Transformable Text IO (Mempool.GenTxId Block) where
  trTransformer = trStructured

instance HasSeverityAnnotation (TraceBenchTxSubmit (Mempool.GenTxId Block))

instance HasPrivacyAnnotation (TraceBenchTxSubmit (Mempool.GenTxId Block))

instance Transformable Text IO (TraceBenchTxSubmit (Mempool.GenTxId Block)) where
  -- transform to JSON Object
  trTransformer = trStructured

instance ToObject TraceLowLevelSubmit where
  toObject MinimalVerbosity _ = emptyObject -- do not log
  toObject NormalVerbosity t =
    case t of
      TraceLowLevelSubmitting -> mkObject ["kind" .= A.String "TraceLowLevelSubmitting"]
      TraceLowLevelAccepted   -> mkObject ["kind" .= A.String "TraceLowLevelAccepted"]
      TraceLowLevelRejected m -> mkObject [ "kind" .= A.String "TraceLowLevelRejected"
                                          , "message" .= A.String (T.pack m)
                                          ]
  toObject MaximalVerbosity t =
    case t of
      TraceLowLevelSubmitting ->
        mkObject [ "kind" .= A.String "TraceLowLevelSubmitting"
                 ]
      TraceLowLevelAccepted ->
        mkObject [ "kind" .= A.String "TraceLowLevelAccepted"
                 ]
      TraceLowLevelRejected errMsg ->
        mkObject [ "kind"   .= A.String "TraceLowLevelRejected"
                 , "errMsg" .= A.String (T.pack errMsg)
                 ]

instance HasSeverityAnnotation TraceLowLevelSubmit

instance HasPrivacyAnnotation TraceLowLevelSubmit

instance (MonadIO m) => Transformable Text m TraceLowLevelSubmit where
  -- transform to JSON Object
  trTransformer = trStructured

type SendRecvTxSubmission blk = TraceSendRecv (TS.TxSubmission (GenTxId blk) (GenTx blk))

instance (Show (TxId (GenTx blk)), Show (GenTx blk), ToJSON (TxId (GenTx blk)))
           => Transformable Text IO (SendRecvTxSubmission blk) where
  -- transform to JSON Object
  trTransformer verb tr = Tracer $ \arg -> do
    currentTime <- getCurrentTime
    let
      obj = toObject verb arg
      updatedObj =
        if obj == emptyObject
          then obj
          else
            -- Add a timestamp in 'ToObject'-representation.
            HM.insert "time" (A.String (T.pack . show $ currentTime)) obj
      tracer = if obj == emptyObject then nullTracer else tr
    meta <- mkLOMeta (getSeverityAnnotation arg) (getPrivacyAnnotation arg)
    traceWith tracer (mempty, LogObject mempty meta (LogStructured updatedObj))

