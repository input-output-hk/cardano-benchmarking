{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-orphans #-}

module Cardano.Benchmarking.Run
  ( parseCommand
  , parserInfo
  , runCommand
  ) where

import Prelude (String, error)
import Prelude qualified
import Data.Version (showVersion)
import Data.Text (pack, unpack)
import Cardano.Prelude hiding (option)
import Control.Monad (fail)
import Control.Monad.Trans.Except.Extra (firstExceptT)
import Control.Tracer (traceWith)
import Options.Applicative qualified as Opt
import Options.Applicative
import Paths_cardano_tx_generator (version)

import Cardano.Chain.Genesis qualified as Genesis

import Ouroboros.Network.Block (MaxSlotNo(..))
import Ouroboros.Network.NodeToClient (IOManager, withIOManager)

import Ouroboros.Consensus.Block.Abstract (BlockProtocol)
import Ouroboros.Consensus.Cardano (Protocol, ProtocolCardano)

import Cardano.Api.Protocol qualified as Api
import Cardano.Api.Typed
import Cardano.Api.TxSubmit
import Cardano.Node.Configuration.Logging
import Cardano.Node.Configuration.POM
import Cardano.Node.Protocol.Cardano
import Cardano.Node.Types

import Cardano.Benchmarking.GeneratorTx
import Cardano.Benchmarking.GeneratorTx.Benchmark
import Cardano.Benchmarking.GeneratorTx.Genesis
import Cardano.Benchmarking.GeneratorTx.CLI.Parsers
import Cardano.Benchmarking.GeneratorTx.Era
import Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition

data ProtocolError =
    IncorrectProtocolSpecified  !Api.Protocol
  | ProtocolInstantiationError  !Text
  | GenesisBenchmarkRunnerError !TxGenError
  | ConfigNotFoundError         !FilePath
  deriving stock Show

data CliError =
    GenesisReadError !FilePath !Genesis.GenesisDataError
  | GenerateTxsError !ProtocolError
  | FileNotFoundError !FilePath
  deriving stock Show

data GeneratorCmd =
  GenerateTxs FilePath
              SocketPath
              AnyCardanoEra
              PartialBenchmark
              (Maybe NetworkMagic)
              Bool
              GeneratorFunds

parserInfo :: String -> Opt.ParserInfo GeneratorCmd
parserInfo t =
  Opt.info
  (parseCommand Opt.<**> Opt.helper)
  (Opt.fullDesc <> Opt.header t)

defaultEra :: AnyCardanoEra
defaultEra = AnyCardanoEra ShelleyEra

parseCommand :: Opt.Parser GeneratorCmd
parseCommand =
  GenerateTxs
    <$> parseConfigFile
          "config"
          "Configuration file for the cardano-node"
    <*> parseSocketPath
          "socket-path"
          "Path to a cardano-node socket"
   <*> ( fromMaybe defaultEra <$>
         (
             eraFlag "shelley" ShelleyEra
         <|> eraFlag "mary"    MaryEra
         <|> eraFlag "allegra" AllegraEra
         )
       )
    <*> parsePartialBenchmark
    <*> optional pMagicOverride
    <*> ( flag False True
          (long "addr-mainnet" <> help "Override address discriminator to mainnet.")
        )
    <*> parseGeneratorFunds
 where
   pMagicOverride :: Opt.Parser NetworkMagic
   pMagicOverride =
     NetworkMagic <$>
     Opt.option Opt.auto
     (  Opt.long "n2n-magic-override"
       <> Opt.metavar "NATURAL"
       <> Opt.help "Override the network magic for the node-to-node protocol."
     )
   eraFlag name tag = flag Nothing (Just $ AnyCardanoEra tag)
                         (long name <> help ("Initialise Cardano in " ++ name ++" submode."))

runCommand :: GeneratorCmd -> ExceptT CliError IO ()
runCommand (GenerateTxs logConfigFp
                        socketFp
                        benchmarkEra
                        cliPartialBenchmark
                        nmagic_opt
                        is_addr_mn
                        fundOptions) =
  withIOManagerE $ \iocp -> do
    benchmark <- case mkBenchmark (defaultBenchmark <> cliPartialBenchmark) of
       Left e -> fail $ "Incomplete benchmark spec (is defaultBenchmark complete?):  " <> unpack e
       Right b -> return b
    let configFp = ConfigYamlFilePath logConfigFp
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
    configYamlPc <- liftIO . parseNodeConfigurationFP . Just $ configFp
    nc <- case makeNodeConfiguration $ configYamlPc <> filesPc of
            Left err -> panic $ "Error in creating the NodeConfiguration: " <> pack err
            Right nc' -> return nc'

    case ncProtocolConfig nc of
      NodeProtocolConfigurationByron _    -> error "NodeProtocolConfigurationByron not supported"
      NodeProtocolConfigurationShelley _  -> error "NodeProtocolConfigurationShelley not supported"
      NodeProtocolConfigurationCardano byC shC hfC -> firstExceptT GenerateTxsError $ do
          ptcl :: Protocol IO CardanoBlock ProtocolCardano <- firstExceptT (ProtocolInstantiationError . pack . show) $
                    mkConsensusProtocolCardano byC shC hfC Nothing
          loggingLayer <- mkLoggingLayer nc ptcl
          let tracers :: BenchTracers IO CardanoBlock
              tracers = createTracers loggingLayer
              myTracer msg = traceWith (btTxSubmit_ tracers) $ TraceBenchTxSubDebug msg

              runAll :: forall era. IsShelleyBasedEra era => Proxy era -> Benchmark -> GeneratorFunds -> ExceptT TxGenError IO ()
              runAll = mangleLocalProtocolDefinition ptcl nmagic_opt is_addr_mn iocp socketFp tracers
          firstExceptT GenesisBenchmarkRunnerError $ case benchmarkEra of
            AnyCardanoEra ByronEra   -> error "ByronEra not supported"
            AnyCardanoEra ShelleyEra -> do
              liftIO $ myTracer "runBenchmark :: ShelleyEra"
              runAll (Proxy @ ShelleyEra) benchmark fundOptions
            AnyCardanoEra AllegraEra -> do
              liftIO $ myTracer "runBenchmark :: AllegraEra"
              runAll (Proxy @ AllegraEra) benchmark fundOptions
            AnyCardanoEra MaryEra    -> do
              liftIO $ myTracer "runBenchmark :: MaryEra"
              runAll (Proxy @ MaryEra) benchmark fundOptions
            _ -> return () -- ???? redundant but type error if left out ??
          liftIO $ do
            threadDelay (200*1000) -- Let the logging layer print out everything.
            shutdownLoggingLayer loggingLayer
 where
   mkLoggingLayer :: NodeConfiguration -> Protocol IO blk (BlockProtocol blk) -> ExceptT ProtocolError IO LoggingLayer
   mkLoggingLayer nc ptcl =
     firstExceptT (\(ConfigErrorFileNotFound fp) -> ConfigNotFoundError fp) $
       createLoggingLayer (pack $ showVersion version) nc ptcl

----------------------------------------------------------------------------

withIOManagerE :: (IOManager -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)

instance Prelude.Show (TxForMode a) where
  show = \case
    TxForByronMode          tx  -> Prelude.show tx
    TxForShelleyMode        tx  -> Prelude.show tx
    TxForCardanoMode (InAnyCardanoEra _ tx)  -> Prelude.show tx
