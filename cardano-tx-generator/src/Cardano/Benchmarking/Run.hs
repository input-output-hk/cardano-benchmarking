{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-orphans #-}

module Cardano.Benchmarking.Run
  (
    plainOldCliScript
  , eraTransitionTest
  ) where

import Cardano.Prelude hiding (option)
--import System.Exit (exitFailure)

import Ouroboros.Network.NodeToClient (IOManager, withIOManager)

import Cardano.Benchmarking.GeneratorTx.Benchmark (GeneratorCmd (..), runParser)
import Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition (CliError(..), runBenchmarkScriptWith)
import qualified Cardano.Benchmarking.Script as Script (plainOldCliScript, eraTransitionTest)

plainOldCliScript :: IO ()
plainOldCliScript = withIOManager $ \iocp -> do
  cmd <- runParser "cardano-tx-generator - load Cardano clusters with parametrised transaction flow"
  runPlainOldCliScript iocp cmd >>= \case
    Right _  -> pure ()
    Left err -> print err >> exitFailure

runPlainOldCliScript :: IOManager -> GeneratorCmd -> IO (Either CliError ())
runPlainOldCliScript
  iocp
  (GenerateTxs
     logConfigFile
     socketFile
     benchmarkEra
     cliPartialBenchmark
     nmagic_opt
     is_addr_mn
     fundOptions
  )
  = runExceptT $ runBenchmarkScriptWith iocp logConfigFile socketFile nmagic_opt is_addr_mn
      $ Script.plainOldCliScript cliPartialBenchmark benchmarkEra fundOptions

eraTransitionTest :: IO ()
eraTransitionTest = withIOManager $ \iocp -> do
  cmd <- runParser "cardano-tx-generator - test era transitioning benchmarks"
  runEraTransitionTest iocp cmd >>= \case
    Right _  -> pure ()
    Left err -> print err >> exitFailure

runEraTransitionTest :: IOManager -> GeneratorCmd -> IO (Either CliError ())
runEraTransitionTest
  iocp
  (GenerateTxs
     logConfigFile
     socketFile
     _benchmarkEra
     cliPartialBenchmark
     nmagic_opt
     is_addr_mn
     fundOptions
  )
  = runExceptT $ runBenchmarkScriptWith iocp logConfigFile socketFile nmagic_opt is_addr_mn
      $ Script.eraTransitionTest cliPartialBenchmark fundOptions
