{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.GeneratorTx.CLI.Run
  ( runCommand
  ) where

import           Prelude (error)
import           Data.Version
                    ( showVersion )
import           Data.Text
                    ( Text, pack )
import           Paths_cardano_tx_generator
                    ( version )
import           Cardano.Prelude hiding (option)
import           Control.Monad
                    ( fail )
import           Control.Monad.Trans.Except.Extra
                    ( firstExceptT, left )

import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Network.Block (MaxSlotNo (..))
import           Ouroboros.Network.NodeToClient
                    ( IOManager
                    , withIOManager
                    )

import qualified Cardano.Chain.Genesis as Genesis

import           Cardano.Api.Protocol (Protocol)
import           Cardano.Config.Types
import           Cardano.Node.Logging
import           Cardano.Node.Protocol
import           Cardano.Node.Protocol.Byron
import           Cardano.Node.Protocol.Shelley
import           Cardano.Node.Types

import           Cardano.Benchmarking.GeneratorTx
import           Cardano.Benchmarking.GeneratorTx.CLI.Parsers
import           Cardano.Benchmarking.GeneratorTx.Error
import           Cardano.Benchmarking.GeneratorTx.Params

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

------------------------------------------------------------------------------------------------

runCommand :: GenerateTxs -> ExceptT CliError IO ()
runCommand (GenerateTxs logConfigFp
                        genFile
                        socketFp
                        b@Benchmark{}
                        sigKeysFiles) =
  withIOManagerE $ \iocp -> do
    -- Logging layer
    loggingLayer <- firstExceptT (\(ConfigErrorFileNotFound fp) -> FileNotFoundError fp) $
                             createLoggingLayer (pack $ showVersion version)
                             ncli

    nc <- liftIO . parseNodeConfigurationFP $ ConfigYamlFilePath logConfigFp

    protocol -- :: Consensus.Protocol IO blk p
        <- firstExceptT GenerateTxsError $
            case ncProtocolConfig nc of
              NodeProtocolConfigurationByron config -> do
                let config' = config { npcByronGenesisFile = genFile }
                firstExceptT (ProtocolInstantiationError . pack . show) $
                  mkSomeConsensusProtocolByron config' Nothing
              NodeProtocolConfigurationShelley config -> do
                let config' = config { npcShelleyGenesisFile = genFile }
                firstExceptT (ProtocolInstantiationError . pack . show) $
                  mkSomeConsensusProtocolShelley config' Nothing
              x -> fail $ "Unsupported protocol: " <> show x

    firstExceptT GenerateTxsError $
        firstExceptT GenesisBenchmarkRunnerError $ do
            case protocol of
              SomeConsensusProtocol p -> do
                let keys = [fp | SigningKeyFile fp <- sigKeysFiles]
                checkBenchmarkParams b keys
                genesisBenchmarkRunner
                  b (mkParams p iocp socketFp loggingLayer) keys

    liftIO $ do
      threadDelay (200*1000) -- Let the logging layer print out everything.
      shutdownLoggingLayer loggingLayer
 where
   mkParams :: Consensus.Protocol IO blk p
            -> IOManager -> SocketPath -> LoggingLayer
            -> Params blk
   mkParams p@Consensus.ProtocolRealPBFT{}   = mkParamsByron   p
   mkParams p@Consensus.ProtocolRealTPraos{} = mkParamsShelley p
   mkParams _ = error $ "Unsupported protocol."

   checkBenchmarkParams :: Benchmark -> [FilePath]
                        -> ExceptT TxGenError IO ()
   checkBenchmarkParams Benchmark{} keys = do
     when (length keys < 3) $
       left $ NeedMinimumThreeSigningKeyFiles keys

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
