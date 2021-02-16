{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition
  (
    CliError (..)
--  , mangleLocalProtocolDefinition
  , runBenchmarkScriptWith
  ) where

import           Prelude (error, show)
import Paths_cardano_tx_generator (version)

import Data.Version (showVersion)
import Data.Text (pack)

import           Cardano.Prelude hiding (TypeError, show)
import Control.Monad.Trans.Except.Extra (firstExceptT)
import Control.Tracer (traceWith)

import           Ouroboros.Consensus.Block.Abstract (BlockProtocol)

import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Cardano (Protocol, ProtocolCardano)

import           Ouroboros.Consensus.Config
                   ( configBlock, configCodec)
import           Ouroboros.Consensus.Config.SupportsNode
                   (ConfigSupportsNode(..), getNetworkMagic)
import           Ouroboros.Consensus.Node.ProtocolInfo
                   (ProtocolInfo (..))
import           Ouroboros.Network.NodeToClient (IOManager)
import           Ouroboros.Network.Block (MaxSlotNo(..))

import           Cardano.Api
import           Cardano.Api.Typed
import qualified Cardano.Api.Typed as Api
import           Cardano.Chain.Slotting
import qualified Cardano.Chain.Genesis as Genesis

import           Cardano.Node.Configuration.Logging
import           Cardano.Node.Configuration.POM
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Types

import Cardano.Benchmarking.DSL -- (BenchmarkScript, DSL(..))
import Cardano.Benchmarking.GeneratorTx.Era
import Cardano.Benchmarking.GeneratorTx.NodeToNode
import qualified Cardano.Benchmarking.GeneratorTx as GeneratorTx
import qualified Cardano.Benchmarking.GeneratorTx.Tx as GeneratorTx

mangleLocalProtocolDefinition ::
     Consensus.Protocol IO blok ptcl
  -> Maybe NetworkMagic
  -> Bool
  -> IOManager
  -> SocketPath
  -> BenchTracers IO CardanoBlock
  -> MonoDSLs
mangleLocalProtocolDefinition ptcl@(Consensus.ProtocolCardano
             _
             Consensus.ProtocolParamsShelleyBased{Consensus.shelleyBasedGenesis}
              _ _ _ _ _ _)
            nmagic_opt is_addr_mn iom (SocketPath sock) tracers
    = (DSL {..}, DSL {..}, DSL {..})
  where

    ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl
    localConnectInfo = LocalNodeConnectInfo
       sock
       (Api.Testnet . getNetworkMagic . configBlock $ pInfoConfig)
       (CardanoMode (EpochSlots 21600))        -- TODO: get this from genesis

    connectClient :: ConnectClient
    connectClient  = benchmarkConnectTxSubmit
                       iom
                       (btConnect_ tracers)
                       (btSubmission_ tracers)
                       (configCodec pInfoConfig)
                       (getNetworkMagic $ configBlock pInfoConfig)

    keyAddress :: IsShelleyBasedEra era => KeyAddress era
    keyAddress = GeneratorTx.keyAddress networkId

    secureGenesisFund :: IsShelleyBasedEra era => SecureGenesisFund era
    secureGenesisFund = GeneratorTx.secureGenesisFund
                (btTxSubmit_ tracers)
                localConnectInfo
                networkId
                shelleyBasedGenesis

    splitFunds :: IsShelleyBasedEra era => SplitFunds era
    splitFunds = GeneratorTx.splitFunds
                (btTxSubmit_ tracers)
                localConnectInfo

    txGenerator :: IsShelleyBasedEra era => TxGenerator era
    txGenerator = GeneratorTx.txGenerator (btTxSubmit_ tracers)

    runBenchmark :: IsShelleyBasedEra era => RunBenchmark era
    runBenchmark = GeneratorTx.runBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient

    networkId = if is_addr_mn
      then Mainnet
      else Testnet $ getNetworkMagic $ configBlock pInfoConfig

mangleLocalProtocolDefinition _ _ _ _ _ _ = error "mkCallbacks"

runBenchmarkScriptWith ::
     IOManager
  -> FilePath
  -> SocketPath
  -> Maybe NetworkMagic
  -> Bool
  -> BenchmarkScript a
  -> ExceptT CliError IO a
runBenchmarkScriptWith iocp logConfigFile socketFile nmagic_opt is_addr_mn script = do
  nc <- liftIO $ mkNodeConfig logConfigFile
  case ncProtocolConfig nc of
    NodeProtocolConfigurationByron _    -> error "NodeProtocolConfigurationByron not supported"
    NodeProtocolConfigurationShelley _  -> error "NodeProtocolConfigurationShelley not supported"
    NodeProtocolConfigurationCardano byC shC hfC -> do
        ptcl :: Protocol IO CardanoBlock ProtocolCardano <- firstExceptT (ProtocolInstantiationError . pack . show) $
                  mkConsensusProtocolCardano byC shC hfC Nothing
        loggingLayer <- mkLoggingLayer nc ptcl
        let tracers :: BenchTracers IO CardanoBlock
            tracers = createTracers loggingLayer
            dslSet :: MonoDSLs
            dslSet = mangleLocalProtocolDefinition ptcl nmagic_opt is_addr_mn iocp socketFile tracers
        res <- firstExceptT BenchmarkRunnerError $ script (tracers, dslSet)
        liftIO $ do
          threadDelay (200*1000) -- Let the logging layer print out everything.
          shutdownLoggingLayer loggingLayer
        return res

  where
    mkLoggingLayer :: NodeConfiguration -> Protocol IO blk (BlockProtocol blk) -> ExceptT CliError IO LoggingLayer
    mkLoggingLayer nc ptcl =
      firstExceptT (\(ConfigErrorFileNotFound fp) -> ConfigNotFoundError fp) $
        createLoggingLayer (pack $ showVersion version) nc ptcl

    mkNodeConfig :: FilePath -> IO NodeConfiguration
    mkNodeConfig logConfig = do
     let configFp = ConfigYamlFilePath logConfig
         filesPc = defaultPartialNodeConfiguration
                   { pncProtocolFiles = Last . Just $
                     ProtocolFilepaths
                     { byronCertFile = Just ""
                     , byronKeyFile = Just ""
                     , shelleyKESFile = Just ""
                     , shelleyVRFFile = Just ""
                     , shelleyCertFile = Just ""
                     , shelleyBulkCredsFile = Just ""
                     }
                   , pncValidateDB = Last $ Just False
                   , pncShutdownIPC = Last $ Just Nothing
                   , pncShutdownOnSlotSynced = Last $ Just NoMaxSlotNo
                   , pncConfigFile = Last $ Just configFp
                   }
     configYamlPc <- parseNodeConfigurationFP . Just $ configFp
     case makeNodeConfiguration $ configYamlPc <> filesPc of
        Left err -> panic $ "Error in creating the NodeConfiguration: " <> pack err
        Right nc' -> return nc'

data CliError  =
    GenesisReadError !FilePath !Genesis.GenesisDataError
  | FileNotFoundError !FilePath
  | ConfigNotFoundError         !FilePath
  | ProtocolInstantiationError  !Text
  | BenchmarkRunnerError !GeneratorTx.TxGenError
  deriving stock Show
