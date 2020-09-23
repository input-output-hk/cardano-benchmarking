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
import           Paths_tx_generator_shelley

import           Cardano.Prelude hiding (option)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Ouroboros.Network.Block (MaxSlotNo (..))
import           Ouroboros.Network.NodeToClient (IOManager, withIOManager)

import           Cardano.Node.Configuration.Logging (createLoggingLayer)
import           Cardano.Node.Configuration.POM

import           Cardano.Node.Types

import           Cardano.Benchmarking.TxGenerator (genesisBenchmarkRunner)
import qualified Cardano.Benchmarking.TxGenerator.CLI.Parsers as P (GenerateTxs (..))
import           Cardano.Benchmarking.TxGenerator.Error (TxGenError)

data CliError = FileNotFoundError !FilePath
    | GenesisBenchmarkRunnerError !TxGenError
    deriving Show

------------------------------------------------------------------------------------------------

runCommand :: P.GenerateTxs -> ExceptT CliError IO ()
runCommand args =
  withIOManagerE $ \iocp -> do
    let configFp = ConfigYamlFilePath $ P.logConfig args
        filesPc = defaultPartialNodeConfiguration
                  { pncProtocolFiles = Last . Just $
                    ProtocolFilepaths
                    { byronCertFile = Just ""
                    , byronKeyFile = Just ""
                    , shelleyKESFile = Just ""
                    , shelleyVRFFile = Just ""
                    , shelleyCertFile = Just ""
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

    loggingLayer <- firstExceptT (\(ConfigErrorFileNotFound fp) -> FileNotFoundError fp) $
                             createLoggingLayer
                                 (pack $ showVersion version)
                                 nc

    firstExceptT GenesisBenchmarkRunnerError $
      genesisBenchmarkRunner args loggingLayer iocp

----------------------------------------------------------------------------

withIOManagerE :: (IOManager -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)
