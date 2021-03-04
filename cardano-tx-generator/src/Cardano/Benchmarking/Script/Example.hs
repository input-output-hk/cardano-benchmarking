module Cardano.Benchmarking.Script.Example
where

import           Prelude
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Word
import qualified Data.ByteString.Lazy as BSL

import           Control.Monad
import           Control.Monad.Trans.RWS.Strict
import           Control.Monad.Trans.Except

import           Data.Dependent.Sum ((==>) )

import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Aeson
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Script.Setters

import           Cardano.Api (AnyCardanoEra(..), CardanoEra(..), Quantity(..), SlotNo(..), quantityToLovelace )
import           Cardano.Node.Types
import           Ouroboros.Network.NodeToClient (withIOManager, IOManager)

runScript :: [Action] -> IOManager -> IO (Either Error ((), ()))
runScript script iom = runExceptT $ evalRWST (forM_ script action) iom emptyEnv

runTestScript s = withIOManager $ runScript s

printJSON :: IO ()
printJSON = BSL.putStrLn $ prettyPrint testScript

txConfig :: [Action]
txConfig = map Set [
    TInitCooldown         ==> InitCooldown 10
  , TNumberOfInputsPerTx  ==> NumberOfInputsPerTx 1
  , TNumberOfOutputsPerTx ==> NumberOfOutputsPerTx 1
  , TNumberOfTxs          ==> NumberOfTxs 500
  , TTPSRate              ==> TPSRate 10
  , TTxAdditionalSize     ==> TxAdditionalSize 0
  , TFee                  ==> (quantityToLovelace $ Quantity 0)
  , TTTL                  ==> SlotNo 1000000
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
  , SecureGenesisFund genFund passPartout passPartout
  , Delay
  , SplitFund outputFunds passPartout genFund
  , Delay
  , SplitFundToList fundList passPartout f1
  , PrepareTxList txList passPartout fundList
  , Set $ TTargets ==> makeTargets [ 3000, 3001, 3002]
  , RunBenchmark txList
  ]
  where
    passPartout = KeyName "pass-partout"
    genFund = FundName "genFund"
    outputFunds = map FundName ["fund1", "fund2", "fund3", "fund4"]
    f1= head outputFunds
    fundList = FundListName "fundList"
    txList = TxListName "txlist"

    makeTargets = NonEmpty.fromList . map (\p -> makeAddr ("127.0.0.1", p))

    makeAddr :: (String, Word16) -> NodeIPv4Address
    makeAddr (a,b) = NodeAddress (NodeHostIPv4Address $ read a) (fromIntegral b)
