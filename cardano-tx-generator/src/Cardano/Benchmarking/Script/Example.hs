module Cardano.Benchmarking.Script.Example
where

import           Prelude
import           Data.Dependent.Sum ((==>) )

import           Control.Monad
import           Control.Monad.Trans.RWS.Strict
import           Control.Monad.Trans.Except

import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Script.Setters

import           Cardano.Api (AnyCardanoEra(..), CardanoEra(..), Quantity(..), SlotNo(..), quantityToLovelace )

test :: IO (Either Error ((), ()))
test = runExceptT $ evalRWST (forM_ testScript action) () emptyEnv

txConfig :: [Action]
txConfig = map Set [
    TInitCooldown         ==> InitCooldown 20
  , TNumberOfInputsPerTx  ==> NumberOfInputsPerTx 1
  , TNumberOfOutputsPerTx ==> NumberOfOutputsPerTx 1
  , TNumberOfTxs          ==> NumberOfTxs 100
  , TTPSRate              ==> TPSRate 10
  , TTxAdditionalSize     ==> TxAdditionalSize 0
  , TFee                  ==> (quantityToLovelace $ Quantity 0)
  , TTTL                  ==> SlotNo 10000
  ]

testScript :: [Action]
testScript =
  txConfig
  ++
  [
    StartProtocol "/work/b1/json/benchmarks/shelley3pools/configuration/configuration-generator.yaml"
  , Set $ TEra ==> AnyCardanoEra MaryEra
  , Set $ TLocalSocket ==> "/work/b1/json/benchmarks/shelley3pools/logs/sockets/1"
  , ReadSigningKey passPartout "/work/b1/json/benchmarks/shelley3pools/configuration/genesis-shelley/utxo-keys/utxo1.skey"
  , KeyAddress addr1 passPartout
  , SecureGenesisFund genFund addr1 passPartout
  , Delay
  , SplitFund outputFunds addr1 genFund passPartout
  ]
  where
    passPartout = KeyName "pass-partout"
    addr1 = AddressName "addr1"
    genFund = FundName "genFund"
    outputFunds = map FundName ["fund1", "fund2", "fund3", "fund4"]
