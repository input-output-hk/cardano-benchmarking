{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Benchmarking.Run
  ( parseCommand
  , parserInfo
  , runCommand
  ) where

import           Prelude (String)
import           Data.Version
                    (showVersion )
import           Data.Text
                    (Text, pack, unpack)
import           Cardano.Prelude hiding (option)
import           Control.Monad
                    (fail)
import           Control.Monad.Trans.Except.Extra
                    (firstExceptT)
import qualified Options.Applicative as Opt
import           Paths_cardano_tx_generator
                    (version)

import qualified Cardano.Chain.Genesis as Genesis

import           Ouroboros.Network.Block (MaxSlotNo (..))
import           Ouroboros.Network.NodeToClient
                    (IOManager, withIOManager)

import           Cardano.Api.Protocol
import           Cardano.Config.Types
import           Cardano.Node.Logging
import           Cardano.Node.Protocol.Byron
import           Cardano.Node.Protocol.Shelley
import           Cardano.Node.Types hiding (Protocol)

import           Cardano.Benchmarking.GeneratorTx
import           Cardano.Benchmarking.GeneratorTx.Benchmark
import           Cardano.Benchmarking.GeneratorTx.CLI.Parsers
import           Cardano.Benchmarking.GeneratorTx.Era


data ProtocolError =
    IncorrectProtocolSpecified  !Protocol
  | ProtocolInstantiationError  !Text
  | GenesisBenchmarkRunnerError !TxGenError
  deriving Show

data CliError =
    GenesisReadError !FilePath !Genesis.GenesisDataError
  | GenerateTxsError !ProtocolError
  | FileNotFoundError !FilePath
  deriving Show

data GeneratorCmd =
  GenerateTxs FilePath
              GenesisFile
              SocketPath
              PartialBenchmark
              SigningKeyFile

parserInfo :: String -> Opt.ParserInfo GeneratorCmd
parserInfo t =
  Opt.info
  (parseCommand Opt.<**> Opt.helper)
  (Opt.fullDesc <> Opt.header t)

parseCommand :: Opt.Parser GeneratorCmd
parseCommand =
  GenerateTxs
    <$> parseConfigFile
          "config"
          "Configuration file for the cardano-node"
    <*> (GenesisFile <$> parseGenesisPath)
    <*> parseSocketPath
          "socket-path"
          "Path to a cardano-node socket"
    <*> parsePartialBenchmark
    <*> parseSigningKeysFile
          "sig-key"
          "Path to signing key file, for genesis UTxO using by generator."

runCommand :: GeneratorCmd -> ExceptT CliError IO ()
runCommand (GenerateTxs logConfigFp
                        genFile
                        socketFp
                        cliPartialBenchmark
                        keyFile) =
  withIOManagerE $ \iocp -> do
    -- Logging layer
    loggingLayer <- firstExceptT (\(ConfigErrorFileNotFound fp) -> FileNotFoundError fp) $
                             createLoggingLayer (pack $ showVersion version)
                             ncli

    nc <- liftIO . parseNodeConfigurationFP $ ConfigYamlFilePath logConfigFp

    p <- firstExceptT GenerateTxsError $
      case ncProtocolConfig nc of
        NodeProtocolConfigurationByron config -> do
          let config' = config { npcByronGenesisFile = genFile }
          ptcl <- firstExceptT (ProtocolInstantiationError . pack . show) $
                    mkConsensusProtocolByron config' Nothing
          pure . SomeEra $ mkEra ptcl iocp socketFp loggingLayer
        NodeProtocolConfigurationShelley config -> do
          let config' = config { npcShelleyGenesisFile = genFile }
          ptcl <- firstExceptT (ProtocolInstantiationError . pack . show) $
                    mkConsensusProtocolShelley config' Nothing
          pure . SomeEra $ mkEra ptcl iocp socketFp loggingLayer
        x -> fail $ "Unsupported protocol: " <> show x

    firstExceptT GenerateTxsError $
      firstExceptT GenesisBenchmarkRunnerError $
        case (p, mkBenchmark
                   (defaultBenchmark <> cliPartialBenchmark)) of
          (_, Left e) -> fail $ "Incomplete benchmark spec (is defaultBenchmark complete?):  " <> unpack e
          (SomeEra era, Right bench) ->
            genesisBenchmarkRunner bench era keyFile
    liftIO $ do
      threadDelay (200*1000) -- Let the logging layer print out everything.
      shutdownLoggingLayer loggingLayer
 where
   ncli :: NodeCLI
   ncli = NodeCLI
          { nodeMode = RealProtocolMode
          , nodeAddr = Nothing
          , configFile = ConfigYamlFilePath logConfigFp
          , topologyFile = TopologyFile "" -- Tx generator doesn't use topology
          , databaseFile = DbFile ""       -- Tx generator doesn't use database
          , socketFile = Just socketFp
          , protocolFiles = ProtocolFilepaths {
               byronCertFile = Just ""
             , byronKeyFile = Just ""
             , shelleyKESFile = Nothing
             , shelleyVRFFile = Nothing
             , shelleyCertFile = Nothing
             }
          , validateDB = False
          , shutdownIPC = Nothing
          , shutdownOnSlotSynced = NoMaxSlotNo
          }

----------------------------------------------------------------------------

withIOManagerE :: (IOManager -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)
