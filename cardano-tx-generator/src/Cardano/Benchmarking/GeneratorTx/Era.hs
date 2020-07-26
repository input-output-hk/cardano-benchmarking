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
  , ModeOfProtocol
  , ConfigSupportsTxGen
  , GenTxOf
  , GenTxIdOf
  , HFCBlockOf
  , inject
  , project
  , SomeMode(..)
  , Mode(..)
  , SomeEra(..)
  , Era(..)
  , ByronBlockHFC
  , ShelleyBlock
  , ShelleyBlockHFC
  , SigningKeyOf
  , SigningKeyRoleOf
  , CardanoBlock

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

  , mkMode
  , modeEra
  , modeCodecConfig
  , modeIOManager
  , modeLedgerConfig
  , modeLocalConnInfo
  , modeNetworkId
  , modeNetworkMagic
  , modeTopLevelConfig
  , modeTracers
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

import           Prelude (Show(..), String, error)
import           Cardano.Prelude hiding (TypeError, show)

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

-- Mode-agnostic imports
import           Cardano.BM.Data.Tracer
                   (emptyObject, mkObject, trStructured)
import           Network.Mux (WithMuxBearer(..))
import           Ouroboros.Consensus.Block.Abstract (CodecConfig)
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
import           Cardano.Config.Types (SocketPath(..))
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

type ModeSupportsTxGen mode =
  ( BenchTraceConstraints (BlockOf mode)
  , Mempool.HasTxId (GenTx (HFCBlockOf mode))
  , Mempool.LedgerSupportsMempool (HFCBlockOf mode)
  , Show (TxSubmitResultForMode mode)
  , Show (TxForMode mode)
  , RunNode (HFCBlockOf mode))

type EraSupportsTxGen era =
  ( Eq (Address era)
  , Key (SigningKeyRoleOf era)
  , Ord (TxOut era)
  , Show (Tx era)
  , Show (TxOut era))

type ConfigSupportsTxGen mode era =
  (ModeSupportsTxGen mode, EraSupportsTxGen era)

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

type ShelleyBlock    = Shelley.ShelleyBlock    TPraosStandardCrypto
type ShelleyBlockHFC = Shelley.ShelleyBlockHFC TPraosStandardCrypto
type CardanoBlock    = Consensus.CardanoBlock  TPraosStandardCrypto

type family BlockOf mode :: Type where
  BlockOf ByronMode      = ByronBlock
  BlockOf ShelleyMode    = ShelleyBlock
  BlockOf CardanoMode    = CardanoBlock
  BlockOf t = TypeError (Ty.Text "Unsupported mode: "  :<>: ShowType t)

type family HFCBlockOf mode :: Type where
  HFCBlockOf ByronMode   = ByronBlockHFC
  HFCBlockOf ShelleyMode = ShelleyBlockHFC
  HFCBlockOf CardanoMode = CardanoBlock
  HFCBlockOf t = TypeError (Ty.Text "Unsupported mode: "  :<>: ShowType t)

type family SigningKeyRoleOf era :: Type where
  SigningKeyRoleOf Byron   = ByronKey
  SigningKeyRoleOf Shelley = PaymentKey
  SigningKeyRoleOf t = TypeError (Ty.Text "Unsupported era: "  :<>: ShowType t)

type SigningKeyOf era = SigningKey (SigningKeyRoleOf era)

type GenTxOf   mode = GenTx   (HFCBlockOf mode)
type GenTxIdOf mode = GenTxId (HFCBlockOf mode)

type family ModeOfProtocol p :: Type where
  ModeOfProtocol Consensus.ProtocolByron   = ByronMode
  ModeOfProtocol Consensus.ProtocolShelley = ShelleyMode
  ModeOfProtocol (HardForkProtocol '[ByronBlock, ShelleyBlock]) = CardanoMode
  ModeOfProtocol t = TypeError (Ty.Text "Unsupported protocol: "  :<>: ShowType t)

data SomeMode =
  forall mode era. ConfigSupportsTxGen mode era =>
  SomeMode (Mode mode era)

-- | System-level submission context (not parameters)
--   TODO:  rename the type to SubContext or something similar.
data Mode mode era where
  ModeByron
    :: TopLevelConfig (HFCBlockOf ByronMode)
    -> CodecConfig (HFCBlockOf ByronMode)
    -> LocalNodeConnectInfo ByronMode (HFCBlockOf ByronMode)
    -> IOManager
    -> BenchTracers IO (HFCBlockOf ByronMode)
    -> Mode ByronMode Byron
  ModeShelley
    :: TopLevelConfig (HFCBlockOf ShelleyMode)
    -> CodecConfig (HFCBlockOf ShelleyMode)
    -> LocalNodeConnectInfo ShelleyMode (HFCBlockOf ShelleyMode)
    -> IOManager
    -> BenchTracers IO (HFCBlockOf ShelleyMode)
    -> Mode ShelleyMode Shelley
  ModeCardanoByron
    :: TopLevelConfig (HFCBlockOf CardanoMode)
    -> CodecConfig (HFCBlockOf CardanoMode)
    -> LocalNodeConnectInfo CardanoMode (HFCBlockOf CardanoMode)
    -> IOManager
    -> BenchTracers IO (HFCBlockOf CardanoMode)
    -> Mode CardanoMode Byron
  ModeCardanoShelley
    :: TopLevelConfig (HFCBlockOf CardanoMode)
    -> CodecConfig (HFCBlockOf CardanoMode)
    -> LocalNodeConnectInfo CardanoMode (HFCBlockOf CardanoMode)
    -> IOManager
    -> BenchTracers IO (HFCBlockOf CardanoMode)
    -> Mode CardanoMode Shelley

instance Show (Mode mode era) where
  show ModeByron{}          = "ModeByron"
  show ModeShelley{}        = "ModeShelley"
  show ModeCardanoByron{}   = "ModeCardanoByron"
  show ModeCardanoShelley{} = "ModeCardanoShelley"

data SomeEra = forall era. EraSupportsTxGen era => SomeEra (Era era)

data Era era where
  EraByron   :: Era Byron
  EraShelley :: Era Shelley

instance Show (Era era) where
  show EraByron   = "EraByron"
  show EraShelley = "EraShelley"

mkMode
  :: forall blok era mode ptcl
  . ( mode ~ ModeOfProtocol ptcl
    )
  => Consensus.Protocol IO blok ptcl
  -> Era era
  -> IOManager
  -> SocketPath
  -> LoggingLayer
  -> Mode mode era
mkMode ptcl@Consensus.ProtocolByron{} EraByron iom (SocketPath sock) ll =
  ModeByron
    pInfoConfig
    (configCodec pInfoConfig)
    (LocalNodeConnectInfo
       sock
       (Api.Testnet . getNetworkMagic . configBlock $ pInfoConfig)
       (ByronMode (Byron.EpochSlots 21600) (SecurityParam 2160)))
    iom
    (createTracers ll)
 where
   ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl
mkMode ptcl@Consensus.ProtocolShelley{} EraShelley iom (SocketPath sock) ll =
  ModeShelley
    pInfoConfig
    (configCodec pInfoConfig)
    (LocalNodeConnectInfo
       sock
       (Api.Testnet . getNetworkMagic . configBlock $ pInfoConfig)
       ShelleyMode)
    iom
    (createTracers ll)
 where
   ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl
mkMode ptcl@Consensus.ProtocolCardano{} EraByron iom (SocketPath sock) ll =
  ModeCardanoByron
    pInfoConfig
    (configCodec pInfoConfig)
    (LocalNodeConnectInfo
       sock
       (Api.Testnet . getNetworkMagic . configBlock $ pInfoConfig)
       (CardanoMode (Byron.EpochSlots 21600) (SecurityParam 2160)))
    iom
    (createTracers ll)
 where
   ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl
mkMode ptcl@Consensus.ProtocolCardano{} EraShelley iom (SocketPath sock) ll =
  ModeCardanoShelley
    pInfoConfig
    (configCodec pInfoConfig)
    (LocalNodeConnectInfo
       sock
       (Api.Testnet . getNetworkMagic . configBlock $ pInfoConfig)
       (CardanoMode (Byron.EpochSlots 21600) (SecurityParam 2160)))
    iom
    (createTracers ll)
 where
   ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl
mkMode p e _ _ _ = error $ "mkMode:  unhandled protocol/era: " <> show p <> " / " <> show e

instance Show (Consensus.Protocol m blk p) where
  show Consensus.ProtocolByron{}   = "ProtocolByron"
  show Consensus.ProtocolShelley{} = "ProtocolShelley"
  show Consensus.ProtocolCardano{} = "ProtocolCardano"
  show Consensus.ProtocolMockBFT{} = "ProtocolMockBFT"
  show Consensus.ProtocolMockPBFT{} = "ProtocolMockPBFT"
  show Consensus.ProtocolMockPraos{} = "ProtocolMockPraos"
  show Consensus.ProtocolLeaderSchedule{} = "ProtocolLeaderSchedule"

modeEra :: Mode mode era -> Era era
modeEra = \case
  ModeByron{}          -> EraByron
  ModeShelley{}        -> EraShelley
  ModeCardanoByron{}   -> EraByron
  ModeCardanoShelley{} -> EraShelley

modeTopLevelConfig :: Mode mode era -> TopLevelConfig (HFCBlockOf mode)
modeTopLevelConfig (ModeByron          x _ _ _ _) = x
modeTopLevelConfig (ModeShelley        x _ _ _ _) = x
modeTopLevelConfig (ModeCardanoByron   x _ _ _ _) = x
modeTopLevelConfig (ModeCardanoShelley x _ _ _ _) = x

modeCodecConfig :: Mode mode era -> CodecConfig (HFCBlockOf mode)
modeCodecConfig    (ModeByron          _ x _ _ _) = x
modeCodecConfig    (ModeShelley        _ x _ _ _) = x
modeCodecConfig    (ModeCardanoByron   _ x _ _ _) = x
modeCodecConfig    (ModeCardanoShelley _ x _ _ _) = x

modeLocalConnInfo :: Mode mode era -> LocalNodeConnectInfo mode (HFCBlockOf mode)
modeLocalConnInfo  (ModeByron          _ _ x _ _) = x
modeLocalConnInfo  (ModeShelley        _ _ x _ _) = x
modeLocalConnInfo  (ModeCardanoByron   _ _ x _ _) = x
modeLocalConnInfo  (ModeCardanoShelley _ _ x _ _) = x

modeIOManager :: Mode mode era -> IOManager
modeIOManager      (ModeByron          _ _ _ x _) = x
modeIOManager      (ModeShelley        _ _ _ x _) = x
modeIOManager      (ModeCardanoByron   _ _ _ x _) = x
modeIOManager      (ModeCardanoShelley _ _ _ x _) = x

modeTracers :: Mode mode era -> BenchTracers IO (HFCBlockOf mode)
modeTracers        (ModeByron          _ _ _ _ x) = x
modeTracers        (ModeShelley        _ _ _ _ x) = x
modeTracers        (ModeCardanoByron   _ _ _ _ x) = x
modeTracers        (ModeCardanoShelley _ _ _ _ x) = x

modeNetworkId :: Mode mode era -> NetworkId
modeNetworkId m@ModeByron{}   = Testnet . getNetworkMagic . configBlock $ modeTopLevelConfig m
modeNetworkId m@ModeShelley{} = Testnet . getNetworkMagic . configBlock $ modeTopLevelConfig m
modeNetworkId m@ModeCardanoByron{}   = Testnet . getNetworkMagic . configBlock $ modeTopLevelConfig m
modeNetworkId m@ModeCardanoShelley{} = Testnet . getNetworkMagic . configBlock $ modeTopLevelConfig m

modeNetworkMagic :: Mode mode era -> NetworkMagic
modeNetworkMagic m@ModeByron{}          = getNetworkMagic . configBlock $ modeTopLevelConfig m
modeNetworkMagic m@ModeShelley{}        = getNetworkMagic . configBlock $ modeTopLevelConfig m
modeNetworkMagic m@ModeCardanoByron{}   = getNetworkMagic . configBlock $ modeTopLevelConfig m
modeNetworkMagic m@ModeCardanoShelley{} = getNetworkMagic . configBlock $ modeTopLevelConfig m

modeLedgerConfig :: Mode mode era -> LedgerConfig (BlockOf mode)
modeLedgerConfig m@ModeByron{}          = configLedger . project $ modeTopLevelConfig m
modeLedgerConfig m@ModeShelley{}        = configLedger . project $ modeTopLevelConfig m
modeLedgerConfig _ = error "Ledger config query not supported in Cardano mode."
-- modeLedgerConfig m@ModeCardanoByron{}   = configLedger . project $ modeTopLevelConfig m
-- modeLedgerConfig m@ModeCardanoShelley{} = configLedger . project $ modeTopLevelConfig m

trBase       :: Mode mode era -> Trace IO Text
trTxSubmit   :: Mode mode era -> Tracer IO (TraceBenchTxSubmit TxId)
trConnect    :: Mode mode era -> Tracer IO SendRecvConnect
trSubmitMux  :: Mode mode era -> Tracer IO (SendRecvTxSubmission (HFCBlockOf mode))
trLowLevel   :: Mode mode era -> Tracer IO TraceLowLevelSubmit
trN2N        :: Mode mode era -> Tracer IO NodeToNodeSubmissionTrace
trBase       = btBase_       . modeTracers
trTxSubmit   = btTxSubmit_   . modeTracers
trConnect    = btConnect_    . modeTracers
trSubmitMux  = btSubmission_ . modeTracers
trLowLevel   = btLowLevel_   . modeTracers
trN2N        = btN2N_        . modeTracers

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
