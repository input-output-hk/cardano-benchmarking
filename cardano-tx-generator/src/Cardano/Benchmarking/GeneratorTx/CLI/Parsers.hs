module Cardano.Benchmarking.GeneratorTx.CLI.Parsers
  ( GenerateTxs (..)
  , parseCommand
  , parseFlag
  ) where

import           Cardano.Prelude hiding (option)
import qualified Data.List.NonEmpty as NE
import           Prelude (String)
import           Options.Applicative
                    ( Parser
                    , bashCompleter, completer, help, long, metavar
                    , auto, flag, option, strOption
                    )
import qualified Control.Arrow as Arr
import           Network.Socket (PortNumber)

import           Cardano.Config.Types
                    ( SigningKeyFile(..)
                    , GenesisFile(..)
                    , SocketPath(..)
                    , NodeAddress(..)
                    , NodeHostAddress(..)
                    )

import           Cardano.Benchmarking.GeneratorTx
import           Cardano.Benchmarking.GeneratorTx.Params

data GenerateTxs =
  GenerateTxs FilePath
              GenesisFile
              SocketPath
              Benchmark
              [SigningKeyFile]


parseCommand :: Parser GenerateTxs
parseCommand =
  GenerateTxs
    <$> parseConfigFile
          "config"
          "Configuration file for the cardano-node"
    <*> (GenesisFile <$> parseGenesisPath)
    <*> parseSocketPath
          "socket-path"
          "Path to a cardano-node socket"
    <*> parseBenchmark
    <*> parseSigningKeysFiles
          "sig-key"
          "Path to signing key file, for genesis UTxO using by generator."

parseBenchmark :: Parser Benchmark
parseBenchmark =
  Benchmark
    <$> (NE.fromList <$> some (
            parseTargetNodeAddress
              "target-node"
              "host and port of the node transactions will be sent to."
          )
        )
    <*> fmap (fromMaybe defaultInitCooldown)
          (optional $ parseInitCooldown
             "init-cooldown"
             "Delay between init and main submission phases.")
    <*> parseNumberOfTxs
          "num-of-txs"
          "Number of transactions generator will create."
    <*> parseTPSRate
          "tps"
          "TPS (transaction per second) rate."
    <*> parseNumberOfInputsPerTx
          "inputs-per-tx"
          "Number of inputs in each of transactions."
    <*> parseNumberOfOutputsPerTx
          "outputs-per-tx"
          "Number of outputs in each of transactions."
    <*> parseFeePerTx
          "tx-fee"
          "Fee per transaction, in Lovelaces."
    <*> optional (
          parseTxAdditionalSize
            "add-tx-size"
            "Additional size of transaction, in bytes."
        )

defaultInitCooldown :: InitCooldown
defaultInitCooldown = InitCooldown 100

----------------------------------------------------------------

parseFlag :: String -> String -> Parser Bool
parseFlag = parseFlag' False True

parseFlag' :: a -> a -> String -> String -> Parser a
parseFlag' def active optname desc =
  flag def active $ long optname <> help desc

parseTargetNodeAddress :: String -> String -> Parser NodeAddress
parseTargetNodeAddress optname desc =
  option
    ( uncurry NodeAddress
      . Arr.first parseHostAddress
      . Arr.second parsePort
      <$> auto
    )
    $ long optname
      <> metavar "(HOST,PORT)"
      <> help desc

parseHostAddress :: String -> NodeHostAddress
parseHostAddress = NodeHostAddress . Just .
  maybe (panic "Bad host of target node") identity . readMaybe

parsePort :: Word16 -> PortNumber
parsePort = fromIntegral

parseNumberOfTxs :: String -> String -> Parser NumberOfTxs
parseNumberOfTxs opt desc = NumberOfTxs <$> parseIntegral opt desc

parseNumberOfInputsPerTx :: String -> String -> Parser NumberOfInputsPerTx
parseNumberOfInputsPerTx opt desc = NumberOfInputsPerTx <$> parseIntegral opt desc

parseNumberOfOutputsPerTx :: String -> String -> Parser NumberOfOutputsPerTx
parseNumberOfOutputsPerTx opt desc = NumberOfOutputsPerTx <$> parseIntegral opt desc

parseFeePerTx :: String -> String -> Parser FeePerTx
parseFeePerTx opt desc = FeePerTx <$> parseIntegral opt desc

parseTPSRate :: String -> String -> Parser TPSRate
parseTPSRate opt desc = TPSRate <$> parseDouble opt desc

parseInitCooldown :: String -> String -> Parser InitCooldown
parseInitCooldown opt desc = InitCooldown <$> parseIntegral opt desc

parseTxAdditionalSize :: String -> String -> Parser TxAdditionalSize
parseTxAdditionalSize opt desc = TxAdditionalSize <$> parseIntegral opt desc

parseSigningKeysFiles :: String -> String -> Parser [SigningKeyFile]
parseSigningKeysFiles opt desc = some $ SigningKeyFile <$> parseFilePath opt desc

------------------------------------------------------------------

parseIntegral :: Integral a => String -> String -> Parser a
parseIntegral optname desc = option (fromInteger <$> auto)
  $ long optname <> metavar "INT" <> help desc

parseDouble :: String -> String -> Parser Double
parseDouble optname desc = option (auto)
  $ long optname <> metavar "DOUBLE" <> help desc

parseFilePath :: String -> String -> Parser FilePath
parseFilePath optname desc =
  strOption
    $ long optname
        <> metavar "FILEPATH"
        <> help desc
        <> completer (bashCompleter "file")

parseSocketPath :: String -> String -> Parser SocketPath
parseSocketPath optname desc =
  SocketPath <$> parseFilePath optname desc

parseConfigFile :: String -> String -> Parser FilePath
parseConfigFile = parseFilePath

parseGenesisPath :: Parser FilePath
parseGenesisPath =
  strOption
    ( long "genesis-file"
        <> metavar "FILEPATH"
        <> help "Path to the genesis yaml file."
    )
