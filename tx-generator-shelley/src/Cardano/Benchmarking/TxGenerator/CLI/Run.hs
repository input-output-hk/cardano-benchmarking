{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Benchmarking.TxGenerator.CLI.Run
  ( runCommand
  ) where

import           Data.Text (pack)
import           Data.Version (showVersion)
import qualified Prelude ()
-- todo: fix
--import           Paths_tx_generator_shelley
--                    ( version )

import           Cardano.Prelude hiding (option)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Ouroboros.Network.Block (MaxSlotNo (..))
import           Ouroboros.Network.NodeToClient (IOManager, withIOManager)

import           Cardano.Node.Logging (createLoggingLayer)

import           Cardano.Config.Types (ConfigError (..), DbFile (..), NodeProtocolMode (..),
                                       ProtocolFilepaths (..), SocketPath (..), TopologyFile (..))
import           Cardano.Node.Types (ConfigYamlFilePath (..), NodeCLI (..))

import           Cardano.Benchmarking.TxGenerator (genesisBenchmarkRunner)
import qualified Cardano.Benchmarking.TxGenerator.CLI.Parsers as P (GenerateTxs (..))
import           Cardano.Benchmarking.TxGenerator.Error (TxGenError)

data CliError
  = FileNotFoundError !FilePath
  | GenesisBenchmarkRunnerError !TxGenError
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
               , socketFile = Just $ SocketPath $ P.socketPath args
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

    loggingLayer <- firstExceptT (\(ConfigErrorFileNotFound fp) -> FileNotFoundError fp) $
                             createLoggingLayer
                                 (pack $ "todo: undefined Version" )--showVersion version)
                                 ncli

    firstExceptT GenesisBenchmarkRunnerError $
      genesisBenchmarkRunner args loggingLayer iocp

----------------------------------------------------------------------------

withIOManagerE :: (IOManager -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)
