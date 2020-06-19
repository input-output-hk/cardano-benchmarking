{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           Ouroboros.Network.Block (MaxSlotNo (..))
import           Ouroboros.Network.NodeToClient
                    ( IOManager
                    , withIOManager
                    )

import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Update (ApplicationName(..))
import           Cardano.Config.Logging
                    ( createLoggingFeature )
import           Cardano.Config.Byron.Protocol
import           Cardano.Config.Types
                    ( DbFile(..), ConfigError(..), ConfigYamlFilePath(..)
                    , CardanoEnvironment(..), NodeByronProtocolConfiguration(..)
                    , ProtocolFilepaths(..), NodeCLI(..)
                    , NodeConfiguration(..), NodeProtocolConfiguration(..)
                    , NodeProtocolMode(..), Protocol, SigningKeyFile(..)
                    , TopologyFile(..), parseNodeConfigurationFP
                    )

import           Cardano.Benchmarking.GeneratorTx.Error
                    ( TxGenError )
import           Cardano.Benchmarking.GeneratorTx.CLI.Parsers
                    ( GenerateTxs (..) )
import           Cardano.Benchmarking.GeneratorTx
                    ( genesisBenchmarkRunner )
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)

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
                        initCooldown
                        txAdditionalSize
                        explorerAPIEndpoint
                        sigKeysFiles) =
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
    -- Default update value
    --let update = Update (ApplicationName "cardano-tx-generator") 1 $ LastKnownBlockVersion 0 2 0
    nc <- liftIO . parseNodeConfigurationFP $ ConfigYamlFilePath logConfigFp
    let NodeProtocolConfigurationByron byroncfg = ncProtocolConfig nc
        byroncfg' = byroncfg { npcByronGenesisFile = genFile
                             --, npcByronApplicationName = Update.ApplicationName "cardano-tx-generator-byron"
                             }
        updatedConfiguration = byroncfg'

    -- Logging layer
    (loggingLayer, _) <- firstExceptT (\(ConfigErrorFileNotFound fp) -> FileNotFoundError fp) $
                             createLoggingFeature
                                 (pack $ showVersion version)
                                 NoEnvironment
                                 ncli

    protocol :: Consensus.Protocol IO ByronBlock Consensus.ProtocolRealPBFT <- firstExceptT GenerateTxsError $
        firstExceptT FromProtocolError $
            mkConsensusProtocolByron
                updatedConfiguration
                Nothing

    firstExceptT GenerateTxsError $
        firstExceptT GenesisBenchmarkRunnerError $
            genesisBenchmarkRunner
                loggingLayer
                iocp
                socketFp
                protocol
                targetNodeAddresses
                numOfTxs
                numOfInsPerTx
                numOfOutsPerTx
                feePerTx
                tps
                initCooldown
                txAdditionalSize
                explorerAPIEndpoint
                [fp | SigningKeyFile fp <- sigKeysFiles]

----------------------------------------------------------------------------

withIOManagerE :: (IOManager -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)
