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
import           Cardano.Config.Types
                    ( DbFile(..), ConfigError(..), ConfigYamlFilePath(..)
                    , CardanoEnvironment(..), CLISocketPath(..)
                    , LastKnownBlockVersion(..), MiscellaneousFilepaths(..)
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

-- TODO(KS): This should probably be imported from Cardano.CLI.Ops
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
    let miscfp = MiscellaneousFilepaths
                 { topFile = TopologyFile "" -- Tx generator doesn't use topology.
                 , dBFile = DbFile ""        -- Tx generator doesn't use database.
                 , delegCertFile = Just delegCert
                 , signKeyFile = Just signingKey
                 , socketFile = Just $ CLISocketPath socketFp
                 }
    let ncli = NodeCLI
               { mscFp = miscfp
               , nodeAddr = NodeAddress (NodeHostAddress Nothing) 19999
               , configFp = ConfigYamlFilePath logConfigFp
               , validateDB = False
               , shutdownIPC = Nothing
               }
    let npm = RealProtocolMode ncli
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
                                 npm

    proto <- firstExceptT GenerateTxsError $
        firstExceptT FromProtocolError $
            mkConsensusProtocolRealPBFT
                updatedConfiguration
                (Just miscfp)

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
