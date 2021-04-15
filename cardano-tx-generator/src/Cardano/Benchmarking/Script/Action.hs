{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Action
where

import           Prelude
import           GHC.Generics
import           Data.Functor.Identity
import           Data.Dependent.Sum (DSum(..))

import           Cardano.Benchmarking.OuroborosImports (SigningKeyFile)
import           Cardano.Api (AnyCardanoEra)

import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Script.Core

data Action where
  Set                :: !SetKeyVal -> Action
--  Declare            :: SetKeyVal   -> Action --declare (once): error if key was set before
  StartProtocol      :: !FilePath -> Action
  Delay              :: Action
  ReadSigningKey     :: !KeyName -> !SigningKeyFile -> Action
  SecureGenesisFund  :: !FundName -> !KeyName -> !KeyName -> Action
  SplitFund          :: [FundName] -> !KeyName -> !FundName -> Action
  SplitFundToList    :: !FundListName -> !KeyName -> !FundName -> Action
  PrepareTxList      :: !TxListName -> !KeyName -> !FundListName -> Action
  RunBenchmark       :: !TxListName -> Action --Obsolete
  AsyncBenchmark     :: !ThreadName -> !TxListName -> Action
  WaitBenchmark      :: !ThreadName -> Action
  CancelBenchmark    :: !ThreadName -> Action
  Reserved           :: [String] -> Action
  WaitForEra         :: !AnyCardanoEra -> Action
  deriving (Show, Eq)

deriving instance Generic Action

action :: Action -> ActionM ()
action a = case a of
  Set (key :=> (Identity val)) -> set (User key) val
  StartProtocol filePath -> startProtocol filePath
  ReadSigningKey name filePath -> readSigningKey name filePath
  SecureGenesisFund fundName fundKey genesisKey -> secureGenesisFund fundName fundKey genesisKey
  SplitFund newFunds newKey sourceFund -> splitFund  newFunds newKey sourceFund
  SplitFundToList fundList destKey sourceFund -> splitFundToList fundList destKey sourceFund
  Delay -> delay
  PrepareTxList name key fund -> prepareTxList name key fund
  RunBenchmark txs -> runBenchmark txs -- Obsolete. Use AsyncBenchmark.
  AsyncBenchmark thread txs -> asyncBenchmark thread txs
  WaitBenchmark thread -> waitBenchmark thread
  CancelBenchmark thread -> cancelBenchmark thread
  WaitForEra era -> waitForEra era
  Reserved options -> reserved options
