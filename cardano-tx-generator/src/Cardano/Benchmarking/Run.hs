{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-orphans #-}

module Cardano.Benchmarking.Run
  ( parseCommand
  , parserInfo
  , runCommand
  ) where

import Prelude (String, id)
import Prelude qualified
import Data.Version (showVersion)
import Data.Text (pack, unpack)
import Cardano.Prelude hiding (option)
import Control.Arrow ((&&&))
import Control.Monad (fail)
import Control.Monad.Trans.Except.Extra (firstExceptT)
import Options.Applicative qualified as Opt
import Paths_cardano_tx_generator (version)

import Cardano.Chain.Genesis qualified as Genesis

import Ouroboros.Network.Block (MaxSlotNo(..))
import Ouroboros.Network.NodeToClient (IOManager, withIOManager)

import Ouroboros.Consensus.Block.Abstract (BlockProtocol)
import Ouroboros.Consensus.Cardano (Protocol, ProtocolByron, ProtocolShelley, ProtocolCardano)

import Cardano.Api.Protocol qualified as Api
import Cardano.Api.Typed
import Cardano.Api.TxSubmit
import Cardano.Node.Configuration.Logging
import Cardano.Node.Configuration.POM
import Cardano.Node.Protocol.Cardano
import Cardano.Node.Protocol.Byron
import Cardano.Node.Protocol.Shelley
import Cardano.Node.Types

import Cardano.Benchmarking.GeneratorTx
import Cardano.Benchmarking.GeneratorTx.Benchmark
import Cardano.Benchmarking.GeneratorTx.Genesis
import Cardano.Benchmarking.GeneratorTx.CLI.Parsers
import Cardano.Benchmarking.GeneratorTx.Era


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
              PartialBenchmark
              SomeEra
              (Maybe NetworkMagic)
              Bool
              GeneratorFunds

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
    <*> parseSocketPath
          "socket-path"
          "Path to a cardano-node socket"
    <*> parsePartialBenchmark
    <*> (fromMaybe (SomeEra defaultEra) <$>
         (   parseFlag' Nothing (Just . SomeEra $ EraByron)
             "byron"   "Initialise Cardano in Byron submode."
         <|> parseFlag' Nothing (Just . SomeEra $ EraShelley)
             "shelley" "Initialise Cardano in Shelley submode."
         ))
    <*> optional pMagicOverride
    <*> parseFlag' False True
          "addr-mainnet"
          "Override address discriminator to mainnet."
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

defaultEra :: Era ShelleyEra
defaultEra = EraShelley

runCommand :: GeneratorCmd -> ExceptT CliError IO ()
runCommand (GenerateTxs logConfigFp
                        socketFp
                        cliPartialBenchmark
                        someEra
                        nmagic_opt
                        is_addr_mn
                        funds) =
  withIOManagerE $ \iocp -> do
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

    (p, loggingLayer) <- firstExceptT GenerateTxsError $
      case ncProtocolConfig nc of
        NodeProtocolConfigurationByron config -> do
          ptcl :: Protocol IO ByronBlockHFC ProtocolByron
               <- firstExceptT (ProtocolInstantiationError . pack . show) $
                    mkConsensusProtocolByron config Nothing
          mkLoggingLayer nc ptcl <&>
            (SomeMode . mkMode ptcl EraByron nmagic_opt is_addr_mn iocp socketFp
             &&& id)
        NodeProtocolConfigurationShelley config -> do
          ptcl :: Protocol IO ShelleyBlockHFC ProtocolShelley
               <- firstExceptT (ProtocolInstantiationError . pack . show) $
                    mkConsensusProtocolShelley config Nothing
          mkLoggingLayer nc ptcl <&>
            (SomeMode . mkMode ptcl EraShelley nmagic_opt is_addr_mn iocp socketFp
             &&& id)
        NodeProtocolConfigurationCardano byC shC hfC -> do
          ptcl :: Protocol IO CardanoBlock ProtocolCardano
               <- firstExceptT (ProtocolInstantiationError . pack . show) $
                    mkConsensusProtocolCardano byC shC hfC Nothing
          case someEra of
            SomeEra era ->
              mkLoggingLayer nc ptcl <&>
                (SomeMode . mkMode ptcl era nmagic_opt is_addr_mn iocp socketFp
                 &&& id)
          -- case someEra of
          --   SomeEra EraByron ->
          --     pure . SomeMode $ mkMode ptcl EraByron iocp socketFp loggingLayer
        -- x -> fail $ "Unsupported protocol: " <> show x

    firstExceptT GenerateTxsError $
      firstExceptT GenesisBenchmarkRunnerError $
        case (p, mkBenchmark
                   (defaultBenchmark <> cliPartialBenchmark)) of
          (_, Left e) -> fail $ "Incomplete benchmark spec (is defaultBenchmark complete?):  " <> unpack e
          (SomeMode mode, Right bench) ->
            secureFunds bench mode funds
            >>= uncurry (runBenchmark bench mode)
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
