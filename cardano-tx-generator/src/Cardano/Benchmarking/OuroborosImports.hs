{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Benchmarking.OuroborosImports
  (
    CardanoBlock
  , Consensus.Protocol
  , Consensus.ProtocolCardano
  , LocalSubmitTx
  , LoggingLayer
  , PaymentKey
  , ShelleyGenesis
  , SigningKey
  , SigningKeyFile
  , StandardShelley
  , NetworkId
  , getGenesis
  , makeLocalConnectInfo
  , protocolToTopLevelConfig
  , protocolToNetworkId
  , protocolToCodecConfig
  , submitTxToNodeLocal
  ) where
import           Prelude

import           Ouroboros.Consensus.Byron.Ledger.Mempool (GenTx)
import           Ouroboros.Consensus.Block.Abstract
import qualified Ouroboros.Consensus.Cardano as Consensus (CardanoBlock, Protocol, ProtocolCardano)
import qualified Ouroboros.Consensus.Cardano as Consensus


import           Ouroboros.Consensus.Config (TopLevelConfig, configBlock, configCodec)
import           Ouroboros.Consensus.Config.SupportsNode
                 (ConfigSupportsNode(..), getNetworkMagic)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Network.NodeToNode -- (Codecs (..), defaultCodecs)
import           Ouroboros.Consensus.Node (ProtocolInfo(..))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)

import           Ouroboros.Network.Channel (Channel (..))
import           Ouroboros.Network.DeltaQ (defaultGSV)
import           Ouroboros.Network.Driver (runPeerWithLimits)
import           Ouroboros.Network.KeepAlive
import           Ouroboros.Network.Magic
import           Ouroboros.Network.Mux (MuxPeer (..), RunMiniProtocol (..), continueForever)
import           Ouroboros.Network.NodeToClient (chainSyncPeerNull, IOManager)
import           Ouroboros.Network.NodeToNode (NetworkConnectTracers (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Client (BlockFetchClient (..),
                                                               blockFetchClientPeer)
import           Ouroboros.Network.Protocol.Handshake.Version (simpleSingletonVersions)
import           Ouroboros.Network.Protocol.KeepAlive.Codec
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import           Ouroboros.Network.Protocol.TxSubmission.Client (TxSubmissionClient,
                                                                 txSubmissionClientPeer)

import           Ouroboros.Network.Snocket (socketSnocket)

import           Cardano.Node.Configuration.Logging (LoggingLayer)

import           Shelley.Spec.Ledger.Genesis (ShelleyGenesis)
import           Cardano.Api (NetworkId(..), LocalNodeConnectInfo(..), ConsensusModeParams(..), EpochSlots(..)
                             , TxInMode, TxValidationErrorInMode
                             , SigningKey, PaymentKey
                             , submitTxToNodeLocal)
import           Cardano.Api.Shelley (CardanoMode)

import           Cardano.CLI.Types (SigningKeyFile)

type CardanoBlock    = Consensus.CardanoBlock StandardCrypto

getGenesis :: Consensus.Protocol IO CardanoBlock ptcl -> ShelleyGenesis StandardShelley
getGenesis
  (Consensus.ProtocolCardano
  _
  Consensus.ProtocolParamsShelleyBased{Consensus.shelleyBasedGenesis}
  _ _ _ _ _ _ ) = shelleyBasedGenesis

protocolToTopLevelConfig :: Consensus.Protocol IO CardanoBlock ptcl -> TopLevelConfig CardanoBlock
protocolToTopLevelConfig ptcl = pInfoConfig
  where ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl

protocolToCodecConfig :: Consensus.Protocol IO CardanoBlock ptcl -> CodecConfig CardanoBlock
protocolToCodecConfig = configCodec . protocolToTopLevelConfig

protocolToNetworkId :: Consensus.Protocol IO CardanoBlock ptcl -> NetworkId
protocolToNetworkId ptcl =
  Testnet $ getNetworkMagic $ configBlock $ protocolToTopLevelConfig ptcl

makeLocalConnectInfo :: NetworkId -> FilePath -> LocalNodeConnectInfo CardanoMode
makeLocalConnectInfo networkId sock
  = LocalNodeConnectInfo
       (CardanoModeParams (EpochSlots 21600))
       networkId
       sock

type LocalSubmitTx = (TxInMode CardanoMode -> IO (SubmitResult (TxValidationErrorInMode CardanoMode)))
