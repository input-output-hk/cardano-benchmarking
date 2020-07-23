{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.GeneratorTx.Era
  ( BlockOf
  , BlockMode
  , EraOfProtocol
  , EraSupportsTxGen
  , GenTxOf
  , GenTxIdOf
  , HFCBlockOf
  , ModeOf
  , SomeEra(..)
  , Era(..)
  , ShelleyBlock
  , ShelleyBlockHFC
  , SigningKeyOf
  , SigningKeyRoleOf

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

  , mkEra
  , eraCodecConfig
  , eraIOManager
  , eraLedgerConfig
  , eraLocalConnInfo
  , eraNetworkId
  , eraNetworkMagic
  , eraTopLevelConfig
  , eraTracers
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

import           Prelude (String, error)
import           Cardano.Prelude hiding (TypeError)

import qualified Codec.CBOR.Term as CBOR
import           Control.Tracer (Tracer (..), nullTracer, traceWith)
import           Cardano.BM.Tracing
import           Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import           Data.Kind (Type)
import qualified Data.Text as T
import           Data.Time.Clock (DiffTime, getCurrentTime)
import           GHC.TypeLits
import qualified GHC.TypeLits as Ty

-- Era-agnostic imports
import           Cardano.BM.Data.Tracer
                   (emptyObject, mkObject, trStructured)
import           Network.Mux (WithMuxBearer(..))
import           Ouroboros.Consensus.Block.Abstract (CodecConfig, SlotNo(..))
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Config
                   ( SecurityParam(..), TopLevelConfig(..)
                   , configBlock, configCodec, configLedger)
import           Ouroboros.Consensus.Config.SupportsNode
                   (ConfigSupportsNode(..), getNetworkMagic)
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig)
import           Ouroboros.Consensus.Ledger.SupportsMempool hiding (TxId)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo
                   (ProtocolInfo (..))
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Network.Driver (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSubmission)
import           Ouroboros.Network.NodeToClient (Handshake, IOManager)
import qualified Ouroboros.Network.NodeToNode as NtN

-- Byron-specific imports
import qualified Cardano.Chain.Slotting as Byron
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Consensus.Cardano.ByronHFC
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Unary

-- Shelley-specific imports
import qualified Ouroboros.Consensus.Cardano.ShelleyHFC as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)

-- Node API imports
import           Cardano.Api.Typed
import           Cardano.Api.TxSubmit
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

import Cardano.Benchmarking.GeneratorTx.Benchmark

{-------------------------------------------------------------------------------
  Era abstraction
-------------------------------------------------------------------------------}
type EraSupportsTxGen era
  = ( Ord (TxOut era)
    , Eq (Address era)
    , Key (SigningKeyRoleOf era)
    , Mempool.HasTxId (GenTx (BlockOf era))
    , RunNode (BlockOf era)
    , Show (Tx era)
    , Show (TxOut era)
    , Show (TxSubmitResultForMode (ModeOf era))
    , BenchTraceConstraints (BlockOf era)
    )

deriving instance Eq (Address era) => Ord (Address era)

deriving instance (Generic TxIn)
deriving instance (Ord TxIn)
instance ToJSON TxIn
instance ToJSON TxId where
  toJSON (TxId x) = toJSON x
instance ToJSON TxIx where
  toJSON (TxIx x) = toJSON x

deriving instance Eq (Address era) => (Eq (TxOut era))
deriving instance Eq (Address era) => (Ord (TxOut era))


type family EraOfProtocol p :: Type where
  EraOfProtocol Consensus.ProtocolByron   = Byron
  EraOfProtocol Consensus.ProtocolShelley = Shelley
  EraOfProtocol t = TypeError (Ty.Text "Unsupported trotocol: "  :<>: ShowType t)

type family ModeOf era :: Type where
  ModeOf Byron       = ByronMode
  ModeOf Shelley     = ShelleyMode
  ModeOf t = TypeError (Ty.Text "Unsupported era: "  :<>: ShowType t)

type ShelleyBlock    = Shelley.ShelleyBlock    TPraosStandardCrypto
type ShelleyBlockHFC = Shelley.ShelleyBlockHFC TPraosStandardCrypto

type family BlockOf era :: Type where
  BlockOf Byron      = ByronBlock
  BlockOf Shelley    = ShelleyBlock
  BlockOf t = TypeError (Ty.Text "Unsupported era: "  :<>: ShowType t)

type family HFCBlockOf era :: Type where
  HFCBlockOf Byron   = ByronBlockHFC
  HFCBlockOf Shelley = HardForkBlock '[ShelleyBlock]
  HFCBlockOf t = TypeError (Ty.Text "Unsupported era: "  :<>: ShowType t)

type family SigningKeyRoleOf era :: Type where
  SigningKeyRoleOf Byron   = ByronKey
  SigningKeyRoleOf Shelley = PaymentKey
  SigningKeyRoleOf t = TypeError (Ty.Text "Unsupported era: "  :<>: ShowType t)

type SigningKeyOf era = SigningKey (SigningKeyRoleOf era)

type GenTxOf   era = GenTx   (BlockOf era)
type GenTxIdOf era = GenTxId (BlockOf era)

type family BlockMode blk :: Type where
  BlockMode ByronBlock   = ByronMode
  BlockMode ShelleyBlock = ShelleyMode
  BlockMode t = TypeError (Ty.Text "Unsupported block type: "  :<>: ShowType t)

data SomeEra =
  forall era. EraSupportsTxGen era =>
  SomeEra (Era era)

-- | System-level submission context (not parameters)
--   TODO:  rename the type to SubContext or something similar.
data Era era where
  EraByron
    :: TopLevelConfig (BlockOf Byron)
    -> CodecConfig (BlockOf Byron)
    -> LocalNodeConnectInfo ByronMode (HFCBlockOf Byron)
    -> IOManager
    -> BenchTracers IO (BlockOf Byron)
    -> Era Byron
  EraShelley
    :: TopLevelConfig (BlockOf Shelley)
    -> CodecConfig (BlockOf Shelley)
    -> LocalNodeConnectInfo ShelleyMode (HFCBlockOf Shelley)
    -> IOManager
    -> BenchTracers IO (BlockOf Shelley)
    -> Era Shelley

mkEra
  :: forall blok era ptcl
  . era ~ EraOfProtocol ptcl
  => Consensus.Protocol IO blok ptcl
  -> IOManager
  -> SocketPath
  -> LoggingLayer
  -> Era (EraOfProtocol ptcl)
mkEra ptcl@Consensus.ProtocolByron{} iom (SocketPath sock) ll =
  EraByron
    (project pInfoConfig)
    (configCodec $ project pInfoConfig)
    (LocalNodeConnectInfo
       sock
       (Api.Testnet . getNetworkMagic . configBlock $ project pInfoConfig)
       (ByronMode (Byron.EpochSlots 21600) (SecurityParam 2160)))
    iom
    (createTracers ll)
 where
   ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl
mkEra ptcl@Consensus.ProtocolShelley{} iom (SocketPath sock) ll =
  EraShelley
    (project pInfoConfig)
    (configCodec $ project pInfoConfig)
    (LocalNodeConnectInfo
       sock
       (Api.Testnet . getNetworkMagic . configBlock $ project pInfoConfig)
       ShelleyMode)
    iom
    (createTracers ll)
 where
   ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl
mkEra _ _ _ _ = error "mkEra:  unhandled protocol"

eraTopLevelConfig :: Era era -> TopLevelConfig (BlockOf era)
eraTopLevelConfig (EraByron   x _ _ _ _) = x
eraTopLevelConfig (EraShelley x _ _ _ _) = x

eraCodecConfig :: Era era -> CodecConfig (BlockOf era)
eraCodecConfig    (EraByron   _ x _ _ _) = x
eraCodecConfig    (EraShelley _ x _ _ _) = x

eraLocalConnInfo :: Era era -> LocalNodeConnectInfo (ModeOf era) (HFCBlockOf era)
eraLocalConnInfo  (EraByron   _ _ x _ _) = x
eraLocalConnInfo  (EraShelley _ _ x _ _) = x

eraIOManager :: Era era -> IOManager
eraIOManager      (EraByron   _ _ _ x _) = x
eraIOManager      (EraShelley _ _ _ x _) = x

eraTracers :: Era era -> BenchTracers IO (BlockOf era)
eraTracers        (EraByron   _ _ _ _ x) = x
eraTracers        (EraShelley _ _ _ _ x) = x

eraNetworkId :: Era era -> NetworkId
eraNetworkId p@EraByron{}   = Testnet . getNetworkMagic . configBlock $ eraTopLevelConfig p
eraNetworkId p@EraShelley{} = Testnet . getNetworkMagic . configBlock $ eraTopLevelConfig p

eraNetworkMagic :: Era era -> NetworkMagic
eraNetworkMagic p@EraByron{}   = getNetworkMagic . configBlock $ eraTopLevelConfig p
eraNetworkMagic p@EraShelley{} = getNetworkMagic . configBlock $ eraTopLevelConfig p

eraLedgerConfig :: Era era -> LedgerConfig (BlockOf era)
eraLedgerConfig = configLedger . eraTopLevelConfig

trBase       :: Era era -> Trace IO Text
trTxSubmit   :: Era era -> Tracer IO (TraceBenchTxSubmit TxId)
trConnect    :: Era era -> Tracer IO SendRecvConnect
trSubmitMux  :: Era era -> Tracer IO (SendRecvTxSubmission (BlockOf era))
trLowLevel   :: Era era -> Tracer IO TraceLowLevelSubmit
trN2N        :: Era era -> Tracer IO NodeToNodeSubmissionTrace
trBase       = btBase_       . eraTracers
trTxSubmit   = btTxSubmit_   . eraTracers
trConnect    = btConnect_    . eraTracers
trSubmitMux  = btSubmission_ . eraTracers
trLowLevel   = btLowLevel_   . eraTracers
trN2N        = btN2N_        . eraTracers

data Benchmark
  = Benchmark
    { bTargets        :: !(NonEmpty NodeAddress)
    , bInitCooldown   :: !InitCooldown
    , bInitialTTL     :: !SlotNo
    , bTxCount        :: !NumberOfTxs
    , bTps            :: !TPSRate
    , bTxFanIn        :: !NumberOfInputsPerTx
    , bTxFanOut       :: !NumberOfOutputsPerTx
    , bTxFee          :: !Lovelace
    , bTxExtraPayload :: !TxAdditionalSize
    }
  deriving (Generic, Show)
-- Warning:  make sure to maintain correspondence between the two data structures.

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
