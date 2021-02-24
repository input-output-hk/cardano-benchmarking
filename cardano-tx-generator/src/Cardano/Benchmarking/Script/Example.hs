module Cardano.Benchmarking.Script.Example
where

import           Prelude
import           Data.Dependent.Sum ((==>) )

import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Setters

defaultScript :: [Action]
defaultScript =
  txConfig
  ++
  [
    StartProtocol "configuration-generator.yaml"
  , Set (TLocalSocket ==> "localSocket")
  , ReadSigningKey "mySigningKey1" "utxo-keys/utxo1.skey"
  ]

txConfig :: [Action]
txConfig = map Set [
    TInitCooldown         ==> InitCooldown 20
  , TNumberOfInputsPerTx  ==> NumberOfInputsPerTx 1
  , TNumberOfOutputsPerTx ==> NumberOfOutputsPerTx 1
  , TNumberOfTxs          ==> NumberOfTxs 100
  , TTPSRate              ==> TPSRate 10
  , TTxAdditionalSize     ==> TxAdditionalSize 0
  ]
