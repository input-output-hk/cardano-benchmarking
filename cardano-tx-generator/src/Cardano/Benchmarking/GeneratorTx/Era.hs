{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.GeneratorTx.Era
  (
    ConfigSupportsTxGen
  , GenTxOf
  , GenTxIdOf
  , CardanoBlock

  , InitCooldown(..)
  , NumberOfInputsPerTx(..)
  , NumberOfOutputsPerTx(..)
  , NumberOfTxs(..)
  , TPSRate(..)

  , Ack(..)
  , Acked(..)
  , ToAnnce(..)
  , Req(..)
  , Sent(..)
  , UnAcked(..)
  , Unav(..)
  , UnReqd(..)

  , SubmissionSummary(..)

  , BenchTraceConstraints
  , BenchTracers(..)
  , NodeToNodeSubmissionTrace(..)
  , TraceBenchTxSubmit(..)
  , TraceLowLevelSubmit(..)
  , createTracers

  , SendRecvConnect
  , SendRecvTxSubmission
  , CardanoMode
  ) where

import           Prelude (Show(..), String)
import           Cardano.Prelude hiding (TypeError, show)

import qualified Codec.CBOR.Term as CBOR
import           Cardano.BM.Tracing
import           Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Time.Clock (DiffTime, getCurrentTime)

-- Mode-agnostic imports
import           Cardano.BM.Data.Tracer
                   (emptyObject, mkObject, trStructured)
import           Network.Mux (WithMuxBearer(..))
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Ledger.SupportsMempool hiding (TxId)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Mempool
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Shelley.Protocol
                   (StandardCrypto)
import           Ouroboros.Network.Driver (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSubmission)
import           Ouroboros.Network.NodeToClient (Handshake)
import qualified Ouroboros.Network.NodeToNode as NtN

-- Node API imports
import           Cardano.Api
import           Cardano.Api.TxSubmit
import           Cardano.Api.Typed

-- Node imports
import           Cardano.Node.Configuration.Logging (LOContent(..), LoggingLayer (..))
import           Cardano.Tracing.OrphanInstances.Byron()
import           Cardano.Tracing.OrphanInstances.Common()
import           Cardano.Tracing.OrphanInstances.Consensus()
import           Cardano.Tracing.OrphanInstances.Network()
import           Cardano.Tracing.OrphanInstances.Shelley()

import Cardano.Benchmarking.GeneratorTx.Benchmark
{-------------------------------------------------------------------------------
  Era abstraction
-------------------------------------------------------------------------------}


type ModeSupportsTxGen mode =
  ( BenchTraceConstraints CardanoBlock
  , Mempool.HasTxId (GenTx CardanoBlock)
  , Mempool.LedgerSupportsMempool CardanoBlock
  , Show (TxSubmitResultForMode mode)
  , RunNode CardanoBlock)

type EraSupportsTxGen era =
  ( IsCardanoEra era
  , Eq (Address era)
  , FromJSON (TxOut era)
  , Key PaymentKey
  , Show (TxOut era)
  , Show (Tx era))

type ConfigSupportsTxGen mode era =
  (ModeSupportsTxGen mode, EraSupportsTxGen era)

-- https://github.com/input-output-hk/cardano-node/issues/1855 would be the proper solution.
deriving stock instance (Generic TxIn)
instance ToJSON TxIn
instance ToJSON TxId where
  toJSON (TxId x) = toJSON x
instance ToJSON TxIx where
  toJSON (TxIx x) = toJSON x

type CardanoBlock    = Consensus.CardanoBlock  StandardCrypto

type GenTxOf   mode = GenTx   CardanoBlock
type GenTxIdOf mode = GenTxId CardanoBlock


instance Show (Consensus.Protocol m blk p) where
  show Consensus.ProtocolByron{}   = "ProtocolByron"
  show Consensus.ProtocolShelley{} = "ProtocolShelley"
  show Consensus.ProtocolCardano{} = "ProtocolCardano"

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}
type BenchTraceConstraints blk =
  ( Show TxId
  , Show (GenTx blk)
  , ToJSON TxId
  , Transformable Text IO (TraceBenchTxSubmit TxId)
  , Transformable Text IO (SendRecvTxSubmission blk)
  )

data BenchTracers m blk =
  BenchTracers
  { btBase_       :: Trace  m Text
  , btTxSubmit_   :: Tracer m (TraceBenchTxSubmit TxId)
  , btConnect_    :: Tracer m SendRecvConnect
  , btSubmission_ :: Tracer m (SendRecvTxSubmission blk)
  , btLowLevel_   :: Tracer m TraceLowLevelSubmit
  , btN2N_        :: Tracer m NodeToNodeSubmissionTrace
  }

createTracers
  :: forall blk
  .  BenchTraceConstraints blk
  => LoggingLayer
  -> BenchTracers IO blk
createTracers loggingLayer =
  BenchTracers
    baseTrace
    benchTracer
    connectTracer
    submitTracer
    lowLevelSubmitTracer
    n2nSubmitTracer
 where
  baseTrace :: Trace IO Text
  baseTrace = llBasicTrace loggingLayer

  tr :: Trace IO Text
  tr = llAppendName loggingLayer "cli" baseTrace

  tr' :: Trace IO Text
  tr' = appendName "generate-txs" tr

  benchTracer :: Tracer IO (TraceBenchTxSubmit TxId)
  benchTracer = toLogObjectVerbose (appendName "benchmark" tr')

  connectTracer :: Tracer IO SendRecvConnect
  connectTracer = toLogObjectVerbose (appendName "connect" tr')

  submitTracer :: Tracer IO (SendRecvTxSubmission blk)
  submitTracer = toLogObjectVerbose (appendName "submit" tr')

  lowLevelSubmitTracer :: Tracer IO TraceLowLevelSubmit
  lowLevelSubmitTracer = toLogObjectVerbose (appendName "llSubmit" tr')

  n2nSubmitTracer :: Tracer IO NodeToNodeSubmissionTrace
  n2nSubmitTracer = toLogObjectVerbose (appendName "submit2" tr')

{-------------------------------------------------------------------------------
  Overall benchmarking trace
-------------------------------------------------------------------------------}
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
  | TraceBenchTxSubServUnav [txid]
  -- ^ Transactions requested, but unavailable in the outstanding set.
  | TraceBenchTxSubServFed [txid] Int
  -- ^ Transactions fed by the feeder, accompanied by sequence number.
  | TraceBenchTxSubServCons [txid]
  -- ^ Transactions consumed by a submitter.
  | TraceBenchTxSubIdle
  -- ^ Remote peer requested new transasctions but none were
  --   available, generator not keeping up?
  | TraceBenchTxSubRateLimit DiffTime
  -- ^ Rate limiter bit, this much delay inserted to keep within
  --   configured rate.
  | TraceBenchTxSubSummary SubmissionSummary
  -- ^ SubmissionSummary.
  | TraceBenchTxSubDebug String
  | TraceBenchTxSubError Text
  deriving stock (Show)

instance Transformable Text IO (TraceBenchTxSubmit TxId) where
  -- transform to JSON Object
  trTransformer = trStructured

instance HasSeverityAnnotation (TraceBenchTxSubmit TxId)
instance HasPrivacyAnnotation  (TraceBenchTxSubmit TxId)

{-------------------------------------------------------------------------------
  N2N submission trace
-------------------------------------------------------------------------------}
data NodeToNodeSubmissionTrace
  = ReqIdsBlocking  Ack Req
  | IdsListBlocking Int

  | ReqIdsPrompt    Ack Req
  | IdsListPrompt   Int

  | ReqTxs          Int
  | TxList          Int

  | EndOfProtocol
  | KThxBye

instance ToObject NodeToNodeSubmissionTrace where
  toObject MinimalVerbosity = const emptyObject -- do not log
  toObject _ = \case
    ReqIdsBlocking  (Ack ack) (Req req) ->
                               mkObject [ "kind" .= A.String "ReqIdsBlocking"
                                        , "ack"  .= A.toJSON ack
                                        , "req"  .= A.toJSON req ]
    IdsListBlocking sent    -> mkObject [ "kind" .= A.String "IdsListBlocking"
                                        , "sent" .= A.toJSON sent ]
    ReqIdsPrompt    (Ack ack) (Req req) ->
                               mkObject [ "kind" .= A.String "ReqIdsPrompt"
                                        , "ack"  .= A.toJSON ack
                                        , "req"  .= A.toJSON req ]
    IdsListPrompt   sent    -> mkObject [ "kind" .= A.String "IdsListPrompt"
                                        , "sent" .= A.toJSON sent ]
    EndOfProtocol           -> mkObject [ "kind" .= A.String "EndOfProtocol" ]
    KThxBye                 -> mkObject [ "kind" .= A.String "KThxBye" ]
    ReqTxs          req     -> mkObject [ "kind" .= A.String "ReqTxs"
                                        , "req"  .= A.toJSON req ]
    TxList          sent    -> mkObject [ "kind" .= A.String "TxList"
                                        , "sent" .= A.toJSON sent ]


instance HasSeverityAnnotation NodeToNodeSubmissionTrace
instance HasPrivacyAnnotation  NodeToNodeSubmissionTrace
instance Transformable Text IO NodeToNodeSubmissionTrace where
  trTransformer = trStructured

{-------------------------------------------------------------------------------
  Low-tevel tracer
-------------------------------------------------------------------------------}
data TraceLowLevelSubmit
  = TraceLowLevelSubmitting
  -- ^ Submitting transaction.
  | TraceLowLevelAccepted
  -- ^ The transaction has been accepted.
  | TraceLowLevelRejected String
  -- ^ The transaction has been rejected, with corresponding error message.
  deriving stock (Show)

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

{-------------------------------------------------------------------------------
  SendRecvTxSubmission
-------------------------------------------------------------------------------}
type SendRecvTxSubmission blk = TraceSendRecv (TxSubmission (GenTxId blk) (GenTx blk))

instance ( Show (GenTx blk)
         , Show (GenTxId blk))
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

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}
instance HasSeverityAnnotation TxId
instance HasPrivacyAnnotation  TxId

instance ToObject TxId where
  toObject MinimalVerbosity _    = emptyObject -- do not log
  toObject NormalVerbosity _     = mkObject [ "kind" .= A.String "GenTxId"]
  toObject MaximalVerbosity txid = mkObject [ "kind" .= A.String "GenTxId"
                                            , "txId" .= toJSON txid
                                            ]

instance Transformable Text IO TxId where
  trTransformer = trStructured

type SendRecvConnect = WithMuxBearer
                         NtN.RemoteConnectionId
                         (TraceSendRecv (Handshake
                                           NtN.NodeToNodeVersion
                                           CBOR.Term))

-- instance {-# OVERLAPS #-} Show (GenTxId blk)
--  => ToJSON (Mempool.GenTxId blk) where
--   toJSON txid = A.String (T.pack $ show txid)

instance ToObject (TraceBenchTxSubmit TxId) where
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
      TraceBenchTxSubServUnav _  -> mkObject ["kind" .= A.String "TraceBenchTxSubServUnav"]
      TraceBenchTxSubServFed _ _ -> mkObject ["kind" .= A.String "TraceBenchTxSubServFed"]
      TraceBenchTxSubServCons _  -> mkObject ["kind" .= A.String "TraceBenchTxSubServCons"]
      TraceBenchTxSubIdle        -> mkObject ["kind" .= A.String "TraceBenchTxSubIdle"]
      TraceBenchTxSubRateLimit _ -> mkObject ["kind" .= A.String "TraceBenchTxSubRateLimit"]
      TraceBenchTxSubSummary _   -> mkObject ["kind" .= A.String "TraceBenchTxSubSummary"]
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
      TraceBenchTxSubServUnav txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServUnav"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServFed txIds ix ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServFed"
                 , "txIds" .= toJSON txIds
                 , "index" .= toJSON ix
                 ]
      TraceBenchTxSubServCons txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServCons"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubIdle ->
        mkObject [ "kind" .= A.String "TraceBenchTxSubIdle"
                 ]
      TraceBenchTxSubRateLimit limit ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubRateLimit"
                 , "limit" .= toJSON limit
                 ]
      TraceBenchTxSubSummary summary ->
        mkObject [ "kind"    .= A.String "TraceBenchTxSubSummary"
                 , "summary" .= toJSON summary
                 ]
      TraceBenchTxSubDebug s ->
        mkObject [ "kind" .= A.String "TraceBenchTxSubDebug"
                 , "msg"  .= A.String (T.pack s)
                 ]
      TraceBenchTxSubError s ->
        mkObject [ "kind" .= A.String "TraceBenchTxSubError"
                 , "msg"  .= A.String s
                 ]
