{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.GeneratorTx.CLI.Run
  ( runCommand
  ) where

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
                    ( firstExceptT )

import           Ouroboros.Network.Block (MaxSlotNo (..))
import           Ouroboros.Network.NodeToClient
                    ( IOManager
                    , withIOManager
                    )

import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Config.Logging
                    ( createLoggingFeature )
import           Cardano.Config.Types
import           Cardano.Node.Protocol
import           Cardano.Node.Protocol.Byron
import           Cardano.Node.Protocol.Shelley
import           Cardano.Shell.Types (CardanoFeature (..))

import           Cardano.Benchmarking.GeneratorTx.Error
                    ( TxGenError )
import           Cardano.Benchmarking.GeneratorTx.CLI.Parsers
                    ( GenerateTxs (..) )
import           Cardano.Benchmarking.GeneratorTx
                    ( genesisBenchmarkRunner )

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
                        signingKey
                        delegCert
                        genFile
                        socketFp
                        targetNodeAddresses
                        numOfTxs
                        numOfInsPerTx
                        numOfOutsPerTx
                        feePerTx
                        tps
                        initCooldown
                        txAdditionalSize
                        sigKeysFiles
                        singleThreaded) =
  withIOManagerE $ \iocp -> do
    let ncli = NodeCLI
               { nodeMode = RealProtocolMode
               , nodeAddr = Nothing
               , configFile = ConfigYamlFilePath logConfigFp
               , topologyFile = TopologyFile "" -- Tx generator doesn't use topology
               , databaseFile = DbFile ""       -- Tx generator doesn't use database
               , socketFile = Just socketFp
               , protocolFiles = ProtocolFilepaths {
                    byronCertFile = Just delegCert
                  , byronKeyFile = Just signingKey
                  , shelleyKESFile = Nothing
                  , shelleyVRFFile = Nothing
                  , shelleyCertFile = Nothing
                  }
               , validateDB = False
               , shutdownIPC = Nothing
               , shutdownOnSlotSynced = NoMaxSlotNo
               }

    -- Logging layer
    (loggingLayer, loggingFeature)
      <- firstExceptT (\(ConfigErrorFileNotFound fp) -> FileNotFoundError fp) $
         createLoggingFeature (pack $ showVersion version) NoEnvironment ncli

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
        firstExceptT GenesisBenchmarkRunnerError $
            case protocol of
              SomeConsensusProtocol p ->
                genesisBenchmarkRunner
                  loggingLayer
                  iocp
                  socketFp
                  p
                  targetNodeAddresses
                  numOfTxs
                  numOfInsPerTx
                  numOfOutsPerTx
                  feePerTx
                  tps
                  initCooldown
                  txAdditionalSize
                  [fp | SigningKeyFile fp <- sigKeysFiles]
                  singleThreaded

    liftIO $ do
      threadDelay (200*1000) -- Let the logging layer print out everything.
      featureShutdown loggingFeature

----------------------------------------------------------------------------

withIOManagerE :: (IOManager -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)
