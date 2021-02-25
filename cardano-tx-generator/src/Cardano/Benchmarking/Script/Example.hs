module Cardano.Benchmarking.Script.Example
where

import           Prelude
import           Data.Dependent.Sum ((==>) )

import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Setters

import           Cardano.Api (AnyCardanoEra(..))
defaultScript :: [Action]
defaultScript =
  txConfig
  ++
  [
    StartProtocol "configuration-generator.yaml"
  , Set $ TEra ==> AnyCardanoEra MaryEra
  , Set $ TLocalSocket ==> "localSocket"
  , ReadSigningKey "mySigningKey1" "utxo-keys/utxo1.skey"
  , KeyAddress "addr1" "mySigningKey1"
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
