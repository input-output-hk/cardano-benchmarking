{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Heredoc
import           Options.Applicative
  
import           Cardano.Benchmarking.Run (parseCommand)


--import           Cardano.Benchmarking.MockServer as MockServer

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =  testGroup "cardano-tx-generator"
  [
    cliArgs    
  , mockServer
  ]
  
mockServer = testGroup "direct/pure client-server connect"
    [ testCase "tx-send == tx-received" $ assertBool "tx-send == tx-received" True -- TODO !
    ]

cliArgs = testGroup "cli arguments"
  [  -- Also update readmes and documentation when the help-messages changes.
     testCase "check help message against pinned version"
     $ assertBool "help message == pinned help message" $ helpMessage == pinnedHelpMessage
  ]
  where
    helpMessage = show $ parserFailure defaultPrefs (info parseCommand fullDesc ) ShowHelpText []
    pinnedHelpMessage = [here|ParserFailure (Usage: <program> --config FILEPATH --socket-path FILEPATH 
                 [--target-node (HOST,PORT)] [--init-cooldown INT] 
                 [--initial-ttl INT] [--num-of-txs INT] [--tps DOUBLE] 
                 [--inputs-per-tx INT] [--outputs-per-tx INT] [--tx-fee INT] 
                 [--add-tx-size INT] [--byron | --shelley] 
                 [--n2n-magic-override NATURAL] [--addr-mainnet] 
                 (--genesis-funds-key FILEPATH | --utxo-funds-key FILEPATH
                   --tx-in TX-IN --tx-out TX-OUT |
                   --split-utxo-funds-key FILEPATH --split-utxo FILEPATH)

Available options:
  --config FILEPATH        Configuration file for the cardano-node
  --socket-path FILEPATH   Path to a cardano-node socket
  --target-node (HOST,PORT)
                           IP address and port of the node transactions will be
                           sent to.
  --init-cooldown INT      Delay between init and main submission phases.
  --initial-ttl INT        Slot denoting TTL of the initial transactions.
  --num-of-txs INT         Number of transactions generator will create.
  --tps DOUBLE             TPS (transaction per second) rate.
  --inputs-per-tx INT      Number of inputs in each of transactions.
  --outputs-per-tx INT     Number of outputs in each of transactions.
  --tx-fee INT             Fee per transaction, in Lovelaces.
  --add-tx-size INT        Additional size of transaction, in bytes.
  --byron                  Initialise Cardano in Byron submode.
  --shelley                Initialise Cardano in Shelley submode.
  --n2n-magic-override NATURAL
                           Override the network magic for the node-to-node
                           protocol.
  --addr-mainnet           Override address discriminator to mainnet.
  --genesis-funds-key FILEPATH
                           Genesis UTxO funds signing key.
  --utxo-funds-key FILEPATH
                           UTxO funds signing key.
  --tx-in TX-IN            The input transaction as TxId#TxIx where TxId is the
                           transaction hash and TxIx is the index.
  --tx-out TX-OUT          The ouput transaction as Address+Lovelace where
                           Address is the Bech32-encoded address followed by the
                           amount in Lovelace.
  --split-utxo-funds-key FILEPATH
                           UTxO funds signing key.
  --split-utxo FILEPATH    UTxO funds file.,ExitSuccess,80)|]
