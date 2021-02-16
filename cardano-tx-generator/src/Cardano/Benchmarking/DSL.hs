{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Benchmarking.DSL
{-
(
  BenchmarkScript
, ScriptM
, DSL(..)
, getDSL
, MonoDSLs
)
-}

where

import           Prelude (error)
import           Cardano.Prelude

import           Cardano.Api
import Cardano.Benchmarking.GeneratorTx.Benchmark -- (Benchmark, GeneratorFunds)
import Cardano.Benchmarking.GeneratorTx.Era
import Cardano.Benchmarking.GeneratorTx
import Cardano.Benchmarking.GeneratorTx.Tx

type ScriptM a = ExceptT TxGenError IO a
type BenchmarkScript a = (BenchTracers IO CardanoBlock, MonoDSLs) ->  ScriptM a

-- Look at hardfork combinator for more elegant abstraction.
type MonoDSLs = (DSL ShelleyEra, DSL AllegraEra, DSL MaryEra)

getDSL :: MonoDSLs -> CardanoEra era -> DSL era
getDSL _         ByronEra = error "ByronEra not supported"
getDSL (x, _, _) ShelleyEra = x
getDSL (_, x, _) AllegraEra = x
getDSL (_, _, x) MaryEra    = x

type Fee = Lovelace
type TTL = SlotNo

type SecureGenesisFund era =
     Fee
  -> TTL
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> ScriptM Fund

type SplitFunds era =
     Lovelace
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> Fund
  -> ScriptM [Fund]

-- txGenerator is basically pure except for logging
type TxGenerator era =
     Benchmark
  -> AddressInEra era
  -> SigningKey PaymentKey
  -> Int
  -> [Fund]
  -> ScriptM [Tx era]

type RunBenchmark era =
     NonEmpty NodeIPv4Address
  -> TPSRate
  -> SubmissionErrorPolicy
  -> [Tx era]
  -> ScriptM ()

type KeyAddress era = SigningKey PaymentKey -> AddressInEra era

data DSL era = DSL {
--    networkId :: NetworkId
    keyAddress :: KeyAddress era
  , secureGenesisFund :: SecureGenesisFund era
  , splitFunds :: SplitFunds era
  , txGenerator :: TxGenerator era
  , runBenchmark :: RunBenchmark era
  }

coolDown :: InitCooldown -> ScriptM ()
coolDown (InitCooldown t) = liftIO $ threadDelay $ 1000 * 1000 * t
