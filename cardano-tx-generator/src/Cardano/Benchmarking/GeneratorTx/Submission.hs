{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.GeneratorTx.Submission
  ( TraceBenchTxSubmit(..)
  , TraceLowLevelSubmit (..)
  , ShelleyBlock
  , BenchTraceConstraints
  , Params(..)
  , mkParamsByron
  , mkParamsShelley
  , paramsConfig
  , trBase
  , trTxSubmit
  , trConnect
  , trSubmitMux
  , trLowLevel
  , trN2N
  , submitTx'
  , NodeToNodeSubmissionTrace(..)
  , TPSRate(..)
  , SubmissionParams(..)
  , Submission
  , SubmissionThreadReport
  , mkSubmission
  , mkSubmissionSummary
  , txSubmissionClient
  , simpleTxFeeder
  , tpsLimitedTxFeeder
  ) where

import           Prelude (fail)
import           Cardano.Prelude hiding (ByteString, atomically, retry, threadDelay)
import           Optics.Setter
import           Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)

import           Control.Arrow ((&&&))
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (STM)
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM as STM
import           Control.Monad (replicateM)
import           Control.Monad.Class.MonadST (MonadST)
import qualified Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Monad.Class.MonadThrow (MonadThrow)

import           Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as A
import           Data.ByteString.Lazy (ByteString)
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.List.NonEmpty as NE
import           Data.String (String)
import qualified Data.Text as T
import           Data.Time.Clock
                   ( DiffTime, NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as Clock
import           Data.Void (Void)

import           Cardano.BM.Tracing
import           Cardano.BM.Data.Tracer (emptyObject, mkObject, trStructured)
import           Control.Tracer (Tracer, traceWith)

import qualified Codec.CBOR.Term as CBOR
import           Network.Mux (MuxTrace(..), WithMuxBearer(..))
import           Ouroboros.Network.NodeToClient (ConnectionId, Handshake, LocalAddress, TraceSendRecv)

import           Cardano.TracingOrphanInstances.Byron()
import           Cardano.TracingOrphanInstances.Common()
import           Cardano.TracingOrphanInstances.Consensus()
import           Cardano.TracingOrphanInstances.Mock()
import           Cardano.TracingOrphanInstances.Network()
import           Cardano.TracingOrphanInstances.Shelley()

import           Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Consensus.Byron.Ledger.Mempool as Mempool (GenTx)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import           Ouroboros.Consensus.Config (TopLevelConfig(..))
import           Ouroboros.Consensus.Ledger.SupportsMempool as Mempool
                   ( GenTxId, HasTxId, TxId, txId, txInBlockSize)
import           Ouroboros.Consensus.Network.NodeToClient
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                  (HasNetworkProtocolVersion (..), nodeToClientProtocolVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Node.Run (RunNode(..))
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Util.Condense (Condense(..))

import           Ouroboros.Network.Mux
                   ( MuxMode(..), OuroborosApplication(..),
                     MuxPeer(..), RunMiniProtocol(..), RunOrStop )
import           Ouroboros.Network.Driver (runPeer)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as LocalTxSub
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))
import           Ouroboros.Network.Protocol.Handshake.Version (Versions)
import           Ouroboros.Network.Protocol.TxSubmission.Client (ClientStIdle(..),
                                                                 ClientStTxs(..),
                                                                 ClientStTxIds(..),
                                                                 TxSubmissionClient(..))
import           Ouroboros.Network.Protocol.TxSubmission.Type (BlockingReplyList(..),
                                                               TokBlockingStyle(..),
                                                               TxSizeInBytes)
import           Ouroboros.Network.NodeToClient (IOManager,
                                                 NetworkConnectTracers(..),
                                                 NodeToClientVersionData(..),
                                                 foldMapVersions,
                                                 versionedNodeToClientProtocols)
import qualified Ouroboros.Network.NodeToClient as NodeToClient

import           Cardano.Config.Logging (LoggingLayer (..))
import           Cardano.Config.Types (SocketPath(..))

import           Cardano.Benchmarking.GeneratorTx.NodeToNode
                    (SendRecvConnect,
                     SendRecvTxSubmission)


{-------------------------------------------------------------------------------
  First, cometh the basic types
-------------------------------------------------------------------------------}

-- | Transactions not yet even announced.
newtype UnReqd  tx = UnReqd  [tx]

-- | Transactions we decided to announce now.
newtype ToAnnce tx = ToAnnce [tx]

-- | Transactions announced, yet unacked by peer.
newtype UnAcked tx = UnAcked [tx]

-- | Transactions acked by peer.
newtype Acked tx = Acked [tx]

-- | Peer acknowledged this many txids of the outstanding window.
newtype Ack = Ack Int deriving (Enum, Eq, Integral, Num, Ord, Real)

-- | Peer requested this many txids to add to the outstanding window.
newtype Req = Req Int deriving (Enum, Eq, Integral, Num, Ord, Real)

-- | This many Txs sent to peer.
newtype Sent = Sent Int deriving (Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

-- | This many Txs requested by the peer, but not available for sending.
newtype Unav = Unav Int deriving (Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

newtype TPSRate = TPSRate Double deriving (Eq, Generic, Ord, Show)

instance ToJSON Sent
instance ToJSON Unav
instance ToJSON TPSRate

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
  | TraceBenchTxSubServFed [txid]
  -- ^ Transactions fed by the feeder.
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
  deriving (Show)

instance HasSeverityAnnotation (TraceBenchTxSubmit (Mempool.GenTxId blk))

instance HasPrivacyAnnotation (TraceBenchTxSubmit (Mempool.GenTxId blk))

instance Show (GenTxId blk) => Transformable Text IO (TraceBenchTxSubmit (Mempool.GenTxId blk)) where
  -- transform to JSON Object
  trTransformer = trStructured

instance {-# OVERLAPS #-} Show (GenTxId blk)
 => ToJSON (Mempool.GenTxId blk) where
  toJSON txid = A.String (T.pack $ show txid)

instance Show (GenTxId blk)
 => ToObject (TraceBenchTxSubmit (Mempool.GenTxId blk)) where
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
      TraceBenchTxSubServFed _   -> mkObject ["kind" .= A.String "TraceBenchTxSubServFed"]
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
      TraceBenchTxSubServFed txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServFed"
                 , "txIds" .= toJSON txIds
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

instance Show (GenTxId blk) => ToObject (Mempool.GenTxId blk) where
  toObject MinimalVerbosity _    = emptyObject -- do not log
  toObject NormalVerbosity _     = mkObject [ "kind" .= A.String "GenTxId"]
  toObject MaximalVerbosity txid = mkObject [ "kind" .= A.String "GenTxId"
                                            , "txId" .= toJSON txid
                                            ]

instance HasSeverityAnnotation (Mempool.GenTxId blk)

instance HasPrivacyAnnotation (Mempool.GenTxId blk)

instance Show (GenTxId blk) => Transformable Text IO (Mempool.GenTxId blk) where
  trTransformer = trStructured

{-------------------------------------------------------------------------------
  N2N submission trace
-------------------------------------------------------------------------------}
instance HasSeverityAnnotation NodeToNodeSubmissionTrace
instance HasPrivacyAnnotation  NodeToNodeSubmissionTrace
instance Transformable Text IO NodeToNodeSubmissionTrace where
  trTransformer = trStructured

-- | Low-tevel tracer
data TraceLowLevelSubmit
  = TraceLowLevelSubmitting
  -- ^ Submitting transaction.
  | TraceLowLevelAccepted
  -- ^ The transaction has been accepted.
  | TraceLowLevelRejected String
  -- ^ The transaction has been rejected, with corresponding error message.
  deriving (Show)

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

type ShelleyBlock = Shelley.ShelleyBlock TPraosStandardCrypto

data BenchTracers m blk =
  BenchTracers
  { btBase_       :: Trace  m Text
  , btTxSubmit_   :: Tracer m (TraceBenchTxSubmit (GenTxId blk))
  , btConnect_    :: Tracer m SendRecvConnect
  , btSubmission_ :: Tracer m (SendRecvTxSubmission blk)
  , btLowLevel_   :: Tracer m TraceLowLevelSubmit
  , btN2N_        :: Tracer m NodeToNodeSubmissionTrace
  }

-- TODO: move out to Types.hs
data Params blk where
  ParamsByron
    :: TopLevelConfig ByronBlock
    -> BenchTracers IO ByronBlock
    -> Params ByronBlock
  ParamsShelley
    :: TopLevelConfig ShelleyBlock
    -> BenchTracers IO ShelleyBlock
    -> Params ShelleyBlock

tracers :: Params blk -> BenchTracers IO blk
tracers (ParamsByron   _ trs) = trs
tracers (ParamsShelley _ trs) = trs

trBase       :: Params blk -> Trace IO Text
trTxSubmit   :: Params blk -> Tracer IO (TraceBenchTxSubmit (GenTxId blk))
trConnect    :: Params blk -> Tracer IO SendRecvConnect
trSubmitMux  :: Params blk -> Tracer IO (SendRecvTxSubmission blk)
trLowLevel   :: Params blk -> Tracer IO TraceLowLevelSubmit
trN2N        :: Params blk -> Tracer IO NodeToNodeSubmissionTrace
trBase       = btBase_       . tracers
trTxSubmit   = btTxSubmit_   . tracers
trConnect    = btConnect_    . tracers
trSubmitMux  = btSubmission_ . tracers
trLowLevel   = btLowLevel_   . tracers
trN2N        = btN2N_        . tracers

mkParamsByron
  :: Consensus.Protocol IO ByronBlock Consensus.ProtocolRealPBFT -> LoggingLayer -> Params ByronBlock
mkParamsByron ptcl = ParamsByron pInfoConfig . createTracers
 where
   ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl

mkParamsShelley
  :: Consensus.Protocol IO ShelleyBlock Consensus.ProtocolRealTPraos -> LoggingLayer -> Params ShelleyBlock
mkParamsShelley ptcl = ParamsShelley pInfoConfig . createTracers
 where
   ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl

type BenchTraceConstraints blk =
  ( Condense (GenTxId blk)
  , Show (GenTxId blk)
  , ToJSON (TxId (GenTx blk))
  , Transformable Text IO (TraceBenchTxSubmit (GenTxId blk))
  , Transformable Text IO (SendRecvTxSubmission blk)
  )

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

  benchTracer :: Tracer IO (TraceBenchTxSubmit (GenTxId blk))
  benchTracer = toLogObjectVerbose (appendName "benchmark" tr')

  connectTracer :: Tracer IO SendRecvConnect
  connectTracer = toLogObjectVerbose (appendName "connect" tr')

  submitTracer :: Tracer IO (SendRecvTxSubmission blk)
  submitTracer = toLogObjectVerbose (appendName "submit" tr')

  lowLevelSubmitTracer :: Tracer IO TraceLowLevelSubmit
  lowLevelSubmitTracer = toLogObjectVerbose (appendName "llSubmit" tr')

  n2nSubmitTracer :: Tracer IO NodeToNodeSubmissionTrace
  n2nSubmitTracer = toLogObjectVerbose (appendName "submit2" tr')

-- TODO: move out to Types.hs
paramsConfig :: Params blk -> TopLevelConfig blk
paramsConfig (ParamsByron   c _) = c
paramsConfig (ParamsShelley c _) = c

-- * Submission
--
submitTx' :: (RunNode blk)
         => Params blk
         -> IOManager
         -> SocketPath
         -> GenTx blk
         -> IO ()
submitTx' p iocp sockpath tx =
    let path = unSocketPath sockpath
        muxTracer :: Show peer => Tracer IO (WithMuxBearer peer MuxTrace)
        muxTracer = toLogObject $ appendName "Mux" (trBase p)
        handshakeTracer :: Tracer IO
                           (WithMuxBearer (ConnectionId LocalAddress)
                            (TraceSendRecv (Handshake NodeToClient.NodeToClientVersion CBOR.Term)))
        handshakeTracer = toLogObject $ appendName "Handshake" (trBase p)
    in NodeToClient.connectTo
         (NodeToClient.localSnocket iocp path)
         NetworkConnectTracers {
             nctMuxTracer       = muxTracer,
             nctHandshakeTracer = handshakeTracer
           }
         (localInitiatorNetworkApplication (trLowLevel p) (paramsConfig p) tx)
         path

localInitiatorNetworkApplication
  :: forall blk m .
     ( RunNode blk
     , MonadST m
     , MonadThrow m
     , MonadTimer m
     )
  => Tracer m TraceLowLevelSubmit
  -> TopLevelConfig blk
  -> GenTx blk
  -> Versions NodeToClient.NodeToClientVersion NodeToClient.DictVersion
              (OuroborosApplication InitiatorMode NodeToClient.LocalAddress ByteString m () Void)

localInitiatorNetworkApplication tracer cfg tx =
  foldMapVersions
    (\v ->
      versionedNodeToClientProtocols
        (nodeToClientProtocolVersion proxy v)
        versionData
        (protocols v))
    (supportedNodeToClientVersions proxy)
 where
    proxy :: Proxy blk
    proxy = Proxy

    versionData = NodeToClientVersionData $ getNetworkMagic $ configBlock cfg

    protocols :: BlockNodeToClientVersion blk
              -> NodeToClient.ConnectionId NodeToClient.LocalAddress
              -> Control.Monad.Class.MonadSTM.STM m RunOrStop
              -> NodeToClient.NodeToClientProtocols InitiatorMode ByteString m () Void
    protocols byronClientVersion _ _=
        NodeToClient.NodeToClientProtocols {
          NodeToClient.localChainSyncProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                nullTracer
                cChainSyncCodec
                NodeToClient.chainSyncPeerNull

        , NodeToClient.localTxSubmissionProtocol =
            InitiatorProtocolOnly $
              MuxPeerRaw $ \channel -> do
                traceWith tracer TraceLowLevelSubmitting
                (result, maybs) <- runPeer
                                     nullTracer -- (contramap show tracer)
                                     cTxSubmissionCodec
                                     channel
                                     (LocalTxSub.localTxSubmissionClientPeer
                                       (txSubmissionClientSingle tx))
                case result of
                  SubmitSuccess -> traceWith tracer TraceLowLevelAccepted
                  SubmitFail msg -> traceWith tracer (TraceLowLevelRejected $ show msg)
                return ((), maybs)

        , NodeToClient.localStateQueryProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                nullTracer
                cStateQueryCodec
                NodeToClient.localStateQueryPeerNull
        }
     where
      Codecs { cChainSyncCodec
             , cTxSubmissionCodec
             , cStateQueryCodec
             } = defaultCodecs (getCodecConfig $ configBlock cfg) byronClientVersion


-- | A 'LocalTxSubmissionClient' that submits exactly one transaction, and then
-- disconnects, returning the confirmation or rejection.
--
txSubmissionClientSingle
  :: forall tx reject m.
     Applicative m
  => tx
  -> LocalTxSub.LocalTxSubmissionClient tx reject m (LocalTxSub.SubmitResult reject)
txSubmissionClientSingle tx = LocalTxSub.LocalTxSubmissionClient $
    pure $ LocalTxSub.SendMsgSubmitTx tx $ \mreject ->
      pure (LocalTxSub.SendMsgDone mreject)

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

{-------------------------------------------------------------------------------
  Parametrisation & state
-------------------------------------------------------------------------------}

data SubmissionParams
  = SubmissionParams
    { spTps         :: !TPSRate
    , spTargets     :: !Natural
    , spQueueLen    :: !Natural
    }

data Submission (m :: Type -> Type) (blk :: Type)
  = Submission
    { sParams      :: !SubmissionParams
    , sStartTime   :: !UTCTime
    , sThreads     :: !Natural
    , sTxSendQueue :: !(TBQueue (Maybe (GenTx blk)))
    , sReportsRefs :: ![STM.TMVar SubmissionThreadReport]
    , sTrace       :: !(Tracer m (TraceBenchTxSubmit (GenTxId blk)))
    }

mkSubmission
  :: MonadIO m
  => Tracer m (TraceBenchTxSubmit (GenTxId blk))
  -> SubmissionParams
  -> m (Submission m blk)
mkSubmission sTrace sParams@SubmissionParams{spTargets=sThreads, spQueueLen} =
 liftIO $ do
  sStartTime <- Clock.getCurrentTime
  sTxSendQueue <- STM.newTBQueueIO spQueueLen
  sReportsRefs <- STM.atomically $ replicateM (fromIntegral sThreads) STM.newEmptyTMVar
  pure Submission{..}

{-------------------------------------------------------------------------------
  Results
-------------------------------------------------------------------------------}

-- | Summary of a submission run.
data SubmissionSummary
  = SubmissionSummary
    { ssTxSent        :: !Sent
    , ssTxUnavailable :: !Unav
    , ssElapsed       :: !NominalDiffTime
    , ssEffectiveTps  :: !TPSRate
    , ssThreadwiseTps :: ![TPSRate]
    } deriving (Show, Generic)
instance ToJSON SubmissionSummary

data SubmissionThreadStats
  = SubmissionThreadStats
    { stsAcked         :: {-# UNPACK #-} !Ack
    , stsSent          :: {-# UNPACK #-} !Sent
    , stsUnavailable   :: {-# UNPACK #-} !Unav
    }

data SubmissionThreadReport
  = SubmissionThreadReport
    { strStats         :: !SubmissionThreadStats
    , strThreadIndex   :: !Natural
    , strEndOfProtocol :: !UTCTime
    }
makeFieldLabelsWith noPrefixFieldLabels ''SubmissionThreadStats

mkSubmissionSummary
  :: MonadIO m
  => Submission m tx
  -> m SubmissionSummary
mkSubmissionSummary
  Submission{ sStartTime, sReportsRefs}
 = do
  reports <- sequence (liftIO . STM.atomically . STM.readTMVar <$> sReportsRefs)
  now <- liftIO Clock.getCurrentTime
  let ssElapsed = Clock.diffUTCTime now sStartTime
      ssTxSent@(Sent sent) = sum $ (stsSent . strStats) <$> reports
      ssTxUnavailable = sum $ (stsUnavailable . strStats) <$> reports
      ssEffectiveTps = txDiffTimeTPS sent ssElapsed
      ssThreadwiseTps = threadReportTps <$> reports
  pure SubmissionSummary{..}
 where
   txDiffTimeTPS :: Int -> NominalDiffTime -> TPSRate
   txDiffTimeTPS n delta =
     TPSRate $ realToFrac $ fromIntegral n / delta

   threadReportTps :: SubmissionThreadReport -> TPSRate
   threadReportTps
     SubmissionThreadReport
       { strStats=SubmissionThreadStats{stsAcked=Ack ack}, strEndOfProtocol } =
         txDiffTimeTPS ack (Clock.diffUTCTime strEndOfProtocol sStartTime)

{-------------------------------------------------------------------------------
  Submission queue:  feeding and consumption
-------------------------------------------------------------------------------}
simpleTxFeeder
  :: forall m blk
  . (MonadIO m, HasTxId (GenTx blk))
  => Submission m blk -> [GenTx blk] -> m ()
simpleTxFeeder
 Submission{sTrace, sThreads, sTxSendQueue} txs = do
  foldM_ (const feedTx) () txs
  -- Issue the termination notifications.
  replicateM_ (fromIntegral sThreads) $
    liftIO $ STM.atomically $ STM.writeTBQueue sTxSendQueue Nothing
 where
   feedTx :: GenTx blk -> m ()
   feedTx tx = do
     liftIO $ STM.atomically $ STM.writeTBQueue sTxSendQueue (Just tx)
     traceWith sTrace $ TraceBenchTxSubServFed [txId tx]

tpsLimitedTxFeeder
  :: forall m blk
  . (MonadIO m)
  => Submission m blk -> [GenTx blk] -> m ()
tpsLimitedTxFeeder
 Submission{ sParams=SubmissionParams{spTps=TPSRate rate}
           , sThreads
           , sTxSendQueue } txs = do
  now <- liftIO Clock.getCurrentTime
  foldM_ feedTx (now, 0) txs
  -- Issue the termination notifications.
  replicateM_ (fromIntegral sThreads) .
    liftIO . STM.atomically $ STM.writeTBQueue sTxSendQueue Nothing
 where
   feedTx :: (UTCTime, NominalDiffTime) -> GenTx blk -> m (UTCTime, NominalDiffTime)
   feedTx (lastPreDelay, lastDelay) tx = do
     liftIO . STM.atomically $ STM.writeTBQueue sTxSendQueue (Just tx)
     now <- liftIO Clock.getCurrentTime
     let targetDelay = realToFrac $ 1.0 / rate
         loopCost = (now `Clock.diffUTCTime` lastPreDelay) - lastDelay
         delay = targetDelay - loopCost
     liftIO . threadDelay . ceiling $ (realToFrac delay * 1000000.0 :: Double)
     pure (now, delay)

consumeTxs
  :: forall m blk
  . (MonadIO m)
  => Submission m blk -> Req -> m (Bool, UnReqd (GenTx blk))
consumeTxs Submission{sTxSendQueue} req
  = liftIO . STM.atomically $ go req []
 where
   go :: Req -> [GenTx blk] -> STM (Bool, UnReqd (GenTx blk))
   go 0 acc = pure (False, UnReqd acc)
   go n acc = STM.readTBQueue sTxSendQueue >>=
              \case
                Nothing -> pure (True, UnReqd acc)
                Just tx -> go (n - 1) (tx:acc)

{-------------------------------------------------------------------------------
  The submission client
-------------------------------------------------------------------------------}
txSubmissionClient
  :: forall m block txid tx .
     ( MonadIO m
     , Mempool.HasTxId (GenTx block)
     , RunNode block
     , txid ~ GenTxId block
     , tx ~ GenTx block
     )
  => Tracer m (TraceBenchTxSubmit txid)
  -> Tracer m NodeToNodeSubmissionTrace
  -> Submission m block
  -> Natural
  -- This return type is forced by Ouroboros.Network.NodeToNode.connectTo
  -> TxSubmissionClient txid tx m ()
txSubmissionClient
    bmtr tr
    sub@Submission{sReportsRefs}
    threadIx =
  TxSubmissionClient $ do
    pure $ client False (UnAcked []) (SubmissionThreadStats 0 0 0)
 where
   -- Nothing means we've ran out of things to either announce or send.
   decideAnnouncement :: TokBlockingStyle a
                      -> Ack -> UnReqd tx -> UnAcked tx
                      -> m (Either Text (ToAnnce tx, UnAcked tx, Acked tx))
   decideAnnouncement b (Ack ack) (UnReqd annNow) (UnAcked unAcked) =
     if   tokIsBlocking b && ack /= length unAcked
     then pure $ Left "decideAnnouncement: TokBlocking, but length unAcked != ack"
     else pure $ Right (ToAnnce annNow, UnAcked newUnacked, Acked acked)
       where
         stillUnacked, newUnacked, acked :: [tx]
         (stillUnacked, acked) = L.splitAtEnd ack unAcked
         newUnacked = annNow <> stillUnacked

   -- Sadly, we can't just return what we want, we instead have to
   -- communicate via IORefs, because..
   client :: Bool -> UnAcked tx -> SubmissionThreadStats
          -- The () return type is forced by Ouroboros.Network.NodeToNode.connectTo
          -> ClientStIdle (TxId (GenTx block)) (GenTx block) m ()
   client done unAcked stats = ClientStIdle
    { recvMsgRequestTxIds = \blocking (protoToAck -> ack) (protoToReq -> req)
       -> do
        traceWith tr $ reqIdsTrace ack req blocking

        (exhausted, unReqd) <-
          if done then pure $ (True, UnReqd [])
          else consumeTxs sub req

        r' <- decideAnnouncement blocking ack unReqd unAcked
        (ann@(ToAnnce annNow), newUnacked@(UnAcked outs), Acked acked)
          <- case r' of
            Left e -> traceWith bmtr (TraceBenchTxSubError e)
                      >> fail (T.unpack e)
            Right x -> pure x

        traceWith tr $ idListTrace ann blocking
        traceWith bmtr $ TraceBenchTxSubServAnn  (txId <$> annNow)
        traceWith bmtr $ TraceBenchTxSubServAck  (txId <$> acked)
        traceWith bmtr $ TraceBenchTxSubServOuts (txId <$> outs)

        let newStats = stats & over #stsAcked (+ ack)

        case (NE.nonEmpty annNow, blocking) of
          (Nothing, TokBlocking) -> do
            traceWith tr EndOfProtocol
            SendMsgDone <$> (submitThreadReport newStats
                             -- The () return type is forced by
                             --   Ouroboros.Network.NodeToNode.connectTo
                             >> pure ())

          (Just neAnnNow, TokBlocking) ->
            pure $ SendMsgReplyTxIds
                     (BlockingReply $ txToIdSizify <$> neAnnNow)
                     (client exhausted newUnacked newStats)

          (_, TokNonBlocking) -> do
            pure $ SendMsgReplyTxIds
                     (NonBlockingReply $ txToIdSizify <$> annNow)
                     (client exhausted newUnacked newStats)

    , recvMsgRequestTxs = \reqTxids -> do
        traceWith tr $ ReqTxs (length reqTxids)
        let UnAcked ua = unAcked
            uaIds = txId <$> ua
            (toSend, _retained) = L.partition ((`L.elem` reqTxids) . txId) ua
            missIds = reqTxids L.\\ uaIds

        traceWith tr $ TxList (length toSend)
        traceWith bmtr $ TraceBenchTxSubServReq reqTxids
        traceWith bmtr $ TraceBenchTxSubServOuts (txId <$> ua)
        unless (L.null missIds) $
          traceWith bmtr $ TraceBenchTxSubServUnav missIds
        pure $ SendMsgReplyTxs toSend
          (client done unAcked $
            stats & over #stsSent        (+ (Sent $ length toSend))
                  & over #stsUnavailable (+ (Unav $ length missIds)))
    , recvMsgKThxBye = do
        traceWith tr KThxBye
        void $ submitThreadReport stats
        pure ()
    }

   submitThreadReport :: SubmissionThreadStats -> m SubmissionThreadReport
   submitThreadReport strStats = do
     strEndOfProtocol <- liftIO Clock.getCurrentTime
     let strThreadIndex = threadIx
         report = SubmissionThreadReport{..}
     liftIO . STM.atomically $ STM.putTMVar (sReportsRefs L.!! fromIntegral threadIx) report
     pure report

   txToIdSizify :: tx -> (TxId tx, TxSizeInBytes)
   txToIdSizify = txId &&& txInBlockSize

   protoToAck :: Word16 -> Ack
   protoToAck = Ack . fromIntegral

   protoToReq :: Word16 -> Req
   protoToReq = Req . fromIntegral

   tokIsBlocking :: TokBlockingStyle a -> Bool
   tokIsBlocking = \case
     TokBlocking    -> True
     TokNonBlocking -> False

   reqIdsTrace :: Ack -> Req -> TokBlockingStyle a -> NodeToNodeSubmissionTrace
   reqIdsTrace ack req = \case
      TokBlocking    -> ReqIdsBlocking ack req
      TokNonBlocking -> ReqIdsPrompt   ack req

   idListTrace :: ToAnnce tx -> TokBlockingStyle a -> NodeToNodeSubmissionTrace
   idListTrace (ToAnnce (length -> toAnn)) = \case
      TokBlocking    -> IdsListBlocking toAnn
      TokNonBlocking -> IdsListPrompt   toAnn
