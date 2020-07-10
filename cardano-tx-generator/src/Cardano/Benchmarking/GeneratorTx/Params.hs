{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.GeneratorTx.Params
  ( Benchmark(..)
  , FeePerTx(..)
  , InitCooldown(..)
  , NumberOfInputsPerTx(..)
  , NumberOfOutputsPerTx(..)
  , NumberOfTxs(..)
  , TxAdditionalSize(..)
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

  , ShelleyBlock
  , BlockMode
  , Params(..)
  , mkParamsByron
  , mkParamsShelley
  , paramsCodecConfig
  , paramsInitialFunds
  , paramsIOManager
  , paramsLedgerConfig
  , paramsLocalConnInfo
  , paramsNetwork
  , paramsNetworkMagic
  , paramsProtocolMagicId
  , paramsTopLevelConfig
  , paramsTracers
  , trBase
  , trTxSubmit
  , trConnect
  , trSubmitMux
  , trLowLevel
  , trN2N

  , BenchTraceConstraints
  , BenchTracers(..)
  , NodeToNodeSubmissionTrace(..)
  , TraceBenchTxSubmit(..)
  , TraceLowLevelSubmit(..)
  , createTracers
  ) where

import           Prelude (String)
import           Cardano.Prelude

import qualified Codec.CBOR.Term as CBOR
import           Control.Tracer (Tracer (..), nullTracer, traceWith)
import           Cardano.BM.Tracing
import           Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import           Data.Kind (Type)
import qualified Data.Text as T
import           Data.Time.Clock (DiffTime, NominalDiffTime, getCurrentTime)
import           Data.Word (Word64)

-- Generic imports
import           Cardano.BM.Data.Tracer
                   (emptyObject, mkObject, trStructured)
import           Cardano.Crypto (ProtocolMagicId)
import           Network.Mux (WithMuxBearer(..))
import           Ouroboros.Consensus.Block.Abstract (CodecConfig)
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Config
                   ( SecurityParam(..), TopLevelConfig(..)
                   , configBlock, configCodec, configLedger)
import           Ouroboros.Consensus.Config.SupportsNode
                   (getNetworkMagic)
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig)
import           Ouroboros.Consensus.Ledger.SupportsMempool
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo
                   (ProtocolInfo (..))
import           Ouroboros.Consensus.Util.Condense (Condense(..))
import           Ouroboros.Network.Driver (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSubmission)
import           Ouroboros.Network.NodeToClient (Handshake, IOManager)
import qualified Ouroboros.Network.NodeToNode as NtN

-- Byron-specific imports
import           Cardano.Chain.Slotting (EpochSlots(..))
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..),
                                                   GenTx (..),
                                                   byronProtocolMagicId)

-- Shelley-specific imports
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Shelley
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.Coin as Shelley

-- Node API imports
import           Cardano.Api hiding (TxId)
import           Cardano.Api.Typed
                   ( LocalNodeConnectInfo(..)
                   , ByronMode, ShelleyMode, NodeConsensusMode(..))
import qualified Cardano.Api.Typed as Api

-- Node imports
import           Cardano.Config.Types (NodeAddress, SocketPath(..))
import           Cardano.Node.Logging (LOContent(..), LoggingLayer (..))
import           Cardano.TracingOrphanInstances.Byron()
import           Cardano.TracingOrphanInstances.Common()
import           Cardano.TracingOrphanInstances.Consensus()
import           Cardano.TracingOrphanInstances.Mock()
import           Cardano.TracingOrphanInstances.Network()
import           Cardano.TracingOrphanInstances.Shelley()


{-------------------------------------------------------------------------------
  Ground types
-------------------------------------------------------------------------------}
newtype FeePerTx =
  FeePerTx Word64
  deriving (Eq, Ord, Show)

-- | How long wait before starting the main submission phase,
--   after the init Tx batch was submitted.
newtype InitCooldown =
  InitCooldown Int
  deriving (Eq, Ord, Show)

newtype NumberOfInputsPerTx =
  NumberOfInputsPerTx Int
  deriving (Eq, Ord, Show)

newtype NumberOfOutputsPerTx =
  NumberOfOutputsPerTx Int
  deriving (Eq, Ord, Show)

newtype NumberOfTxs =
  NumberOfTxs Word64
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

data Benchmark
  = Benchmark
    { bTargets        :: NonEmpty NodeAddress
    , bInitCooldown   :: InitCooldown
    , bTxCount        :: NumberOfTxs
    , bTps            :: TPSRate
    , bTxFanIn        :: NumberOfInputsPerTx
    , bTxFanOut       :: NumberOfOutputsPerTx
    , bTxFee          :: FeePerTx
    , bTxExtraPayload :: Maybe TxAdditionalSize
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  System abstraction
-------------------------------------------------------------------------------}
type ShelleyBlock = Shelley.ShelleyBlock TPraosStandardCrypto

-- | System-level submission context (not parameters)
--   TODO:  rename the type to SubContext or something similar.
data Params blk where
  ParamsByron
    :: TopLevelConfig ByronBlock
    -> CodecConfig ByronBlock
    -> LocalNodeConnectInfo ByronMode ByronBlock
    -> IOManager
    -> BenchTracers IO ByronBlock
    -> Params ByronBlock
  ParamsShelley
    :: TopLevelConfig ShelleyBlock
    -> CodecConfig ShelleyBlock
    -> LocalNodeConnectInfo ShelleyMode ShelleyBlock
    -> IOManager
    -> BenchTracers IO ShelleyBlock
    -> Params ShelleyBlock

mkParamsByron
  :: Consensus.Protocol IO ByronBlock Consensus.ProtocolRealPBFT
  -> IOManager
  -> SocketPath
  -> LoggingLayer
  -> Params ByronBlock
mkParamsByron ptcl iom (SocketPath sock) =
  ParamsByron
    pInfoConfig
    (configCodec pInfoConfig)
    (LocalNodeConnectInfo
      { localNodeSocketPath    = sock
      , localNodeNetworkId     = Api.Testnet . getNetworkMagic . configBlock $ pInfoConfig
      , localNodeConsensusMode = ByronMode
                                 (EpochSlots 21600)
                                 (SecurityParam 2160)
      })
    iom
  . createTracers
 where
   ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl

mkParamsShelley
  :: Consensus.Protocol IO ShelleyBlock Consensus.ProtocolRealTPraos
  -> IOManager
  -> SocketPath
  -> LoggingLayer
  -> Params ShelleyBlock
mkParamsShelley ptcl iom (SocketPath sock) =
  ParamsShelley
    pInfoConfig
    (configCodec pInfoConfig)
    (LocalNodeConnectInfo
      { localNodeSocketPath    = sock
      , localNodeNetworkId     = Api.Testnet . getNetworkMagic . configBlock $ pInfoConfig
      , localNodeConsensusMode = ShelleyMode
      })
    iom
  . createTracers
 where
   ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl

type family BlockMode blk :: Type where
  BlockMode ByronBlock   = ByronMode
  BlockMode ShelleyBlock = ShelleyMode

paramsTopLevelConfig :: Params blk -> TopLevelConfig blk
paramsTopLevelConfig (ParamsByron   x _ _ _ _) = x
paramsTopLevelConfig (ParamsShelley x _ _ _ _) = x

paramsCodecConfig :: Params blk -> CodecConfig blk
paramsCodecConfig    (ParamsByron   _ x _ _ _) = x
paramsCodecConfig    (ParamsShelley _ x _ _ _) = x

paramsLocalConnInfo :: Params blk -> LocalNodeConnectInfo (BlockMode blk) blk
paramsLocalConnInfo  (ParamsByron   _ _ x _ _) = x
paramsLocalConnInfo  (ParamsShelley _ _ x _ _) = x

paramsIOManager :: Params blk -> IOManager
paramsIOManager      (ParamsByron   _ _ _ x _) = x
paramsIOManager      (ParamsShelley _ _ _ x _) = x

paramsTracers :: Params blk -> BenchTracers IO blk
paramsTracers        (ParamsByron   _ _ _ _ x) = x
paramsTracers        (ParamsShelley _ _ _ _ x) = x

paramsInitialFunds :: Params ShelleyBlock
                   -> Map (Shelley.Addr TPraosStandardCrypto) Shelley.Coin
paramsInitialFunds = Consensus.sgInitialFunds . Shelley.shelleyLedgerGenesis . paramsLedgerConfig

paramsNetworkMagic :: Params blk -> NetworkMagic
paramsNetworkMagic p@ParamsByron{}   = getNetworkMagic . configBlock $ paramsTopLevelConfig p
paramsNetworkMagic p@ParamsShelley{} = getNetworkMagic . configBlock $ paramsTopLevelConfig p

paramsNetwork :: Params blk -> Network
paramsNetwork = Testnet . paramsNetworkMagic

paramsProtocolMagicId :: Params ByronBlock -> ProtocolMagicId
paramsProtocolMagicId p@ParamsByron{} = byronProtocolMagicId . configBlock $ paramsTopLevelConfig p

paramsLedgerConfig :: Params blk -> LedgerConfig blk
paramsLedgerConfig = configLedger . paramsTopLevelConfig

trBase       :: Params blk -> Trace IO Text
trTxSubmit   :: Params blk -> Tracer IO (TraceBenchTxSubmit (GenTxId blk))
trConnect    :: Params blk -> Tracer IO SendRecvConnect
trSubmitMux  :: Params blk -> Tracer IO (SendRecvTxSubmission blk)
trLowLevel   :: Params blk -> Tracer IO TraceLowLevelSubmit
trN2N        :: Params blk -> Tracer IO NodeToNodeSubmissionTrace
trBase       = btBase_       . paramsTracers
trTxSubmit   = btTxSubmit_   . paramsTracers
trConnect    = btConnect_    . paramsTracers
trSubmitMux  = btSubmission_ . paramsTracers
trLowLevel   = btLowLevel_   . paramsTracers
trN2N        = btN2N_        . paramsTracers

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}
type BenchTraceConstraints blk =
  ( Condense (GenTxId blk)
  , Show (GenTxId blk)
  , ToJSON (TxId (GenTx blk))
  , Transformable Text IO (TraceBenchTxSubmit (GenTxId blk))
  , Transformable Text IO (SendRecvTxSubmission blk)
  )

data BenchTracers m blk =
  BenchTracers
  { btBase_       :: Trace  m Text
  , btTxSubmit_   :: Tracer m (TraceBenchTxSubmit (GenTxId blk))
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

instance Show (GenTxId blk) => Transformable Text IO (TraceBenchTxSubmit (Mempool.GenTxId blk)) where
  -- transform to JSON Object
  trTransformer = trStructured

instance HasSeverityAnnotation (TraceBenchTxSubmit (Mempool.GenTxId blk))
instance HasPrivacyAnnotation  (TraceBenchTxSubmit (Mempool.GenTxId blk))

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

{-------------------------------------------------------------------------------
  SendRecvTxSubmission
-------------------------------------------------------------------------------}
type SendRecvTxSubmission blk = TraceSendRecv (TxSubmission (GenTxId blk) (GenTx blk))

-- instance ( Show (GenTxId blk)
--          , ToJSON (TxId (GenTx blk)))
--  => ToObject (SendRecvTxSubmission blk) where
--   toObject MinimalVerbosity _ = emptyObject -- do not log
--   toObject NormalVerbosity t =
--     case t of
--       TraceSendMsg (AnyMessage msg) ->
--         case msg of
--           TS.MsgRequestTxIds _ _ _                 -> mkObject ["kind" .= String "TxSubmissionSendRequestTxIds"]
--           TS.MsgReplyTxIds (TS.BlockingReply _)    -> mkObject ["kind" .= String "TxSubmissionSendBReplyTxIds"]
--           TS.MsgReplyTxIds (TS.NonBlockingReply _) -> mkObject ["kind" .= String "TxSubmissionSendNBReplyTxIds"]
--           TS.MsgRequestTxs _                       -> mkObject ["kind" .= String "TxSubmissionSendRequestTxs"]
--           TS.MsgReplyTxs _                         -> mkObject ["kind" .= String "TxSubmissionSendReplyTxs"]
--           TS.MsgKThxBye                            -> mkObject ["kind" .= String "MsgKThxBye"]
--           TS.MsgDone                               -> emptyObject -- No useful information.
--       TraceRecvMsg (AnyMessage msg) ->
--         case msg of
--           TS.MsgRequestTxIds _ _ _                 -> mkObject ["kind" .= String "TxSubmissionRecvRequestTxIds"]
--           TS.MsgReplyTxIds (TS.BlockingReply _)    -> mkObject ["kind" .= String "TxSubmissionRecvBReplyTxIds"]
--           TS.MsgReplyTxIds (TS.NonBlockingReply _) -> mkObject ["kind" .= String "TxSubmissionRecvNBReplyTxIds"]
--           TS.MsgRequestTxs _                       -> mkObject ["kind" .= String "TxSubmissionRecvRequestTxs"]
--           TS.MsgReplyTxs _                         -> mkObject ["kind" .= String "TxSubmissionRecvReplyTxs"]
--           TS.MsgKThxBye                            -> mkObject ["kind" .= String "MsgKThxBye"]
--           TS.MsgDone                               -> emptyObject -- No useful information.

--   toObject MaximalVerbosity t =
--     case t of
--       TraceSendMsg (AnyMessage msg) ->
--         case msg of
--           TS.MsgRequestTxIds _ ackNumber reqNumber ->
--             mkObject [ "kind"   .= String "TxSubmissionSendRequestTxIds"
--                      , "ackNum" .= Number (fromIntegral ackNumber)
--                      , "reqNum" .= Number (fromIntegral reqNumber)
--                      ]
--           TS.MsgReplyTxIds (TS.BlockingReply txIds) ->
--             mkObject [ "kind"  .= String "TxSubmissionSendBReplyTxIds"
--                      , "txIds" .= toJSON txIds
--                      ]
--           TS.MsgReplyTxIds (TS.NonBlockingReply txIds) ->
--             mkObject [ "kind"  .= String "TxSubmissionSendNBReplyTxIds"
--                      , "txIds" .= toJSON txIds
--                      ]
--           TS.MsgRequestTxs txIds ->
--             mkObject [ "kind"  .= String "TxSubmissionSendRequestTxs"
--                      , "txIds" .= toJSON txIds
--                      ]
--           TS.MsgReplyTxs _ -> -- We shouldn't log a list of whole transactions here.
--             mkObject [ "kind" .= String "TxSubmissionSendReplyTxs" ]
--           TS.MsgKThxBye ->
--             mkObject [ "kind" .= String "MsgKThxBye" ]
--           TS.MsgDone -> emptyObject -- No useful information.

--       TraceRecvMsg (AnyMessage msg) ->
--         case msg of
--           TS.MsgRequestTxIds _ ackNumber reqNumber ->
--             mkObject [ "kind"   .= String "TxSubmissionRecvRequestTxIds"
--                      , "ackNum" .= Number (fromIntegral ackNumber)
--                      , "reqNum" .= Number (fromIntegral reqNumber)
--                      ]
--           TS.MsgReplyTxIds (TS.BlockingReply txIds) ->
--             mkObject [ "kind"  .= String "TxSubmissionRecvBReplyTxIds"
--                      , "txIds" .= toJSON txIds
--                      ]
--           TS.MsgReplyTxIds (TS.NonBlockingReply txIds) ->
--             mkObject [ "kind"  .= String "TxSubmissionRecvNBReplyTxIds"
--                      , "txIds" .= toJSON txIds
--                      ]
--           TS.MsgRequestTxs txIds ->
--             mkObject [ "kind"  .= String "TxSubmissionRecvRequestTxs"
--                      , "txIds" .= toJSON txIds
--                      ]
--           TS.MsgReplyTxs _ -> -- We shouldn't log a list of whole transactions here.
--             mkObject [ "kind" .= String "TxSubmissionRecvReplyTxs" ]
--           TS.MsgKThxBye ->
--             mkObject [ "kind" .= String "MsgKThxBye" ]
--           TS.MsgDone -> emptyObject -- No useful information.

-- instance HasSeverityAnnotation (SendRecvTxSubmission blk)
-- instance HasPrivacyAnnotation (SendRecvTxSubmission blk)

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
instance HasSeverityAnnotation (Mempool.GenTxId blk)
instance HasPrivacyAnnotation  (Mempool.GenTxId blk)

instance Show (GenTxId blk)
 => ToObject (Mempool.GenTxId blk) where
  toObject MinimalVerbosity _    = emptyObject -- do not log
  toObject NormalVerbosity _     = mkObject [ "kind" .= A.String "GenTxId"]
  toObject MaximalVerbosity txid = mkObject [ "kind" .= A.String "GenTxId"
                                            , "txId" .= toJSON txid
                                            ]

instance Show (GenTxId blk)
 => Transformable Text IO (Mempool.GenTxId blk) where
  trTransformer = trStructured

type SendRecvConnect = WithMuxBearer
                         NtN.RemoteConnectionId
                         (TraceSendRecv (Handshake
                                           NtN.NodeToNodeVersion
                                           CBOR.Term))

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
