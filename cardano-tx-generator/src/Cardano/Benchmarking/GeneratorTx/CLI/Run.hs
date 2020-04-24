{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Benchmarking.GeneratorTx.CLI.Run
  ( runCommand
  ) where

import           Data.Version
                    ( showVersion )
import           Data.Text
                    ( pack )
import           Paths_cardano_tx_generator
                    ( version )
import           Cardano.Prelude hiding (option)
import           Control.Monad.Trans.Except.Extra
                    ( firstExceptT )

import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Network.NodeToClient
                    ( IOManager
                    , withIOManager
                    )

import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Update (ApplicationName(..))
import           Cardano.Config.Logging
                    ( createLoggingFeature )
import           Cardano.Config.Protocol.Byron
                    ( ByronProtocolInstantiationError(..)
                    , mkConsensusProtocolRealPBFT )
import           Cardano.Config.Protocol.Types (SomeConsensusProtocol(..))
import           Cardano.Config.Types
                    ( DbFile(..), ConfigError(..), ConfigYamlFilePath(..)
                    , CardanoEnvironment(..), CLISocketPath(..)
                    , LastKnownBlockVersion(..), ProtocolFilepaths(..)
                    , NodeAddress(..), NodeCLI(..), NodeConfiguration(..)
                    , NodeHostAddress(..), NodeProtocolMode(..), Protocol
                    , SigningKeyFile(..), TopologyFile(..), Update(..)
                    , parseNodeConfigurationFP
                    )

import           Cardano.Benchmarking.GeneratorTx.Error
                    ( TxGenError )
import           Cardano.Benchmarking.GeneratorTx.CLI.Parsers
                    ( GenerateTxs (..) )
import           Cardano.Benchmarking.GeneratorTx
                    ( genesisBenchmarkRunner )

data RealPBFTError =
    IncorrectProtocolSpecified !Protocol
  | FromProtocolError !ByronProtocolInstantiationError
  | GenesisBenchmarkRunnerError !TxGenError
  deriving Show

data CliError =
    GenesisReadError !FilePath !Genesis.GenesisDataError
  | GenerateTxsError !RealPBFTError
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
                        txAdditionalSize
                        explorerAPIEndpoint
                        sigKeysFiles) =
  withIOManagerE $ \iocp -> do
    let ncli = NodeCLI
               { nodeMode = RealProtocolMode
               , nodeAddr = NodeAddress (NodeHostAddress Nothing) 19999
               , configFile = ConfigYamlFilePath logConfigFp
               , topologyFile = TopologyFile "" -- Tx generator doesn't use topology
               , databaseFile = DbFile ""       -- Tx generator doesn't use database
               , socketFile = Just $ CLISocketPath socketFp
               , protocolFiles = ProtocolFilepaths {
                    byronCertFile = Just delegCert
                  , byronKeyFile = Just signingKey
                  , shelleyKESFile = Nothing
                  , shelleyVRFFile = Nothing
                  , shelleyCertFile = Nothing
                  }
               , validateDB = False
               , shutdownIPC = Nothing
               }
    -- Default update value
    let update = Update (ApplicationName "cardano-tx-generator") 1 $ LastKnownBlockVersion 0 2 0
    nc <- liftIO . parseNodeConfigurationFP $ ConfigYamlFilePath logConfigFp
    let updatedConfiguration :: NodeConfiguration
        updatedConfiguration = nc { ncGenesisFile = genFile
                                  , ncUpdate = update
                                  }
    -- Logging layer
    (loggingLayer, _) <- firstExceptT (\(ConfigErrorFileNotFound fp) -> FileNotFoundError fp) $
                             createLoggingFeature
                                 (pack $ showVersion version)
                                 NoEnvironment
                                 ncli

    (SomeConsensusProtocol proto @Consensus.ProtocolRealPBFT{}) <- firstExceptT GenerateTxsError $
        firstExceptT FromProtocolError $
            mkConsensusProtocolRealPBFT
                updatedConfiguration
                Nothing

    firstExceptT GenerateTxsError $
        firstExceptT GenesisBenchmarkRunnerError $
            genesisBenchmarkRunner
                loggingLayer
                iocp
                socketFp
                proto
                targetNodeAddresses
                numOfTxs
                numOfInsPerTx
                numOfOutsPerTx
                feePerTx
                tps
                txAdditionalSize
                explorerAPIEndpoint
                [fp | SigningKeyFile fp <- sigKeysFiles]

----------------------------------------------------------------------------

withIOManagerE :: (IOManager -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)
