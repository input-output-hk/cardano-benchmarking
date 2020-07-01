{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Benchmarking.TxGenerator.CLI.Run
  ( runCommand
  ) where

import qualified Prelude ()
import           Data.Version
                    ( showVersion )
import           Data.Text
                    ( pack )
-- todo: fix
--import           Paths_tx_generator_shelley
--                    ( version )

import           Cardano.Prelude hiding (option)
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
import           Cardano.Node.Protocol.Shelley
                       ( ShelleyProtocolInstantiationError(..))

import           Cardano.Config.Types
                    ( DbFile(..), ConfigError(..), ConfigYamlFilePath(..)
                    , CardanoEnvironment(..)
                    , ProtocolFilepaths(..), NodeCLI(..)
                    , NodeProtocolMode(..), Protocol
                    , TopologyFile(..)
                    )

import           Cardano.Benchmarking.TxGenerator.Error
                    ( TxGenError )
import qualified Cardano.Benchmarking.TxGenerator.CLI.Parsers as P
                    ( GenerateTxs (..) )
import           Cardano.Benchmarking.TxGenerator
                    ( genesisBenchmarkRunner )

data RealPBFTError =
    IncorrectProtocolSpecified !Protocol
  | FromProtocolError !ShelleyProtocolInstantiationError
  | GenesisBenchmarkRunnerError !TxGenError
  deriving Show

data CliError =
    GenesisReadError !FilePath !Genesis.GenesisDataError
  | GenerateTxsError !RealPBFTError
  | FileNotFoundError !FilePath
  deriving Show

------------------------------------------------------------------------------------------------

runCommand :: P.GenerateTxs -> ExceptT CliError IO ()
runCommand args =
  withIOManagerE $ \iocp -> do
    let ncli = NodeCLI
               { nodeMode = RealProtocolMode
               , nodeAddr = Nothing
               , configFile = ConfigYamlFilePath $ P.logConfig args
               , topologyFile = TopologyFile "" -- Tx generator doesn't use topology
               , databaseFile = DbFile ""       -- Tx generator doesn't use database
               , socketFile = Just $ P.socketPath args
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

    (loggingLayer, _) <- firstExceptT (\(ConfigErrorFileNotFound fp) -> FileNotFoundError fp) $
                             createLoggingFeature
                                 (pack $ "todo: undefined Version" )--showVersion version)
                                 NoEnvironment
                                 ncli

    firstExceptT GenerateTxsError $
        firstExceptT GenesisBenchmarkRunnerError $
            genesisBenchmarkRunner args loggingLayer iocp


----------------------------------------------------------------------------

withIOManagerE :: (IOManager -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)
