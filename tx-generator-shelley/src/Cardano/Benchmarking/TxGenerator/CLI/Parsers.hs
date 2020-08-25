{-# LANGUAGE OverloadedStrings #-}
module Cardano.Benchmarking.TxGenerator.CLI.Parsers
  ( GenerateTxs (..)
  , InitialFund (..)
  , parseCommand
  ) where

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Char8 as BSC
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import           Options.Applicative as Opt (Parser, ReadM, auto, bashCompleter, completer,
                                             eitherReader, flag', help, long, metavar, option,
                                             strOption)

import qualified Control.Arrow as Arr
import           Network.Socket (PortNumber)

import           Cardano.Node.Types (NodeAddress (..), NodeHostAddress (..))

import           Cardano.Api.Typed
import           Cardano.CLI.Shelley.Parsers (parseTxIn)

import           Cardano.Benchmarking.TxGenerator.Types


data GenerateTxs = GenerateTxs
    { network       :: NetworkId
    , logConfig     :: FilePath
    , socketPath    :: FilePath
    , nodeAdresses  :: (NonEmpty NodeAddress)
    , txCount       :: NumberOfTxs
    --  , txinputs      :: NumberOfInputsPerTx
    , fee           :: FeePerTx
    , tps           :: TPSRate
    , coolDownDelay :: InitCoolDown
    , extraPayload  :: (Maybe TxAdditionalSize)
    , explorerAPI   :: (Maybe ExplorerAPIEnpoint)
    , initialFund   :: InitialFund
    }

parseCommand :: Parser GenerateTxs
parseCommand =
  GenerateTxs
    <$> pNetwork
    <*> parseFilePath
          "config"
          "a cardano-node config file (for logging config)"
    <*> parseFilePath
          "socket-path"
          "Path to a cardano-node socket"
    <*> (NE.fromList <$> some (
            parseTargetNodeAddress
              "target-node"
              "host and port of the node transactions will be sent to."
          )
        )
    <*> parseNumberOfTxs
          "num-of-txs"
          "Number of transactions generator will create."
{-
    <*> parseNumberOfInputsPerTx
          "inputs-per-tx"
          "Number of inputs in each of transactions."
    <*> parseNumberOfOutputsPerTx
          "outputs-per-tx"
          "Number of outputs in each of transactions."
-}
  <*> parseFeePerTx
          "tx-fee"
          "Fee per transaction, in Lovelaces."
    <*> parseTPSRate
          "tps"
          "TPS (transaction per second) rate."
    <*> fmap (fromMaybe defaultInitCooldown)
          (optional $ parseInitCooldown
             "init-cooldown"
             "Delay between init and main submission phases.")
    <*> optional (
          parseTxAdditionalSize
            "add-tx-size"
            "Additional size of transaction, in bytes."
        )
    <*> optional (
          parseExplorerAPIEndpoint
            "submit-to-api"
            "Explorer's API endpoint to submit transaction."
        )
    <*>  parseInitialFund

defaultInitCooldown :: InitCoolDown
defaultInitCooldown = InitCoolDown 100

----------------------------------------------------------------

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

parseFeePerTx :: String -> String -> Parser FeePerTx
parseFeePerTx opt desc = FeePerTx <$> parseIntegral opt desc

parseTPSRate :: String -> String -> Parser TPSRate
parseTPSRate opt desc = TPSRate <$> parseFloat opt desc

parseInitCooldown :: String -> String -> Parser InitCoolDown
parseInitCooldown opt desc = InitCoolDown <$> parseIntegral opt desc

parseTxAdditionalSize :: String -> String -> Parser TxAdditionalSize
parseTxAdditionalSize opt desc = TxAdditionalSize <$> parseIntegral opt desc

parseExplorerAPIEndpoint :: String -> String -> Parser ExplorerAPIEnpoint
parseExplorerAPIEndpoint opt desc = ExplorerAPIEnpoint <$> parseUrl opt desc

------------------------------------------------------------------

parseIntegral :: Integral a => String -> String -> Parser a
parseIntegral optname desc = option (fromInteger <$> auto)
  $ long optname <> metavar "INT" <> help desc

parseFloat :: String -> String -> Parser Float
parseFloat optname desc = option (auto)
  $ long optname <> metavar "FLOAT" <> help desc

parseUrl :: String -> String -> Parser String
parseUrl optname desc =
  strOption $ long optname <> metavar "URL" <> help desc

parseFilePath :: String -> String -> Parser FilePath
parseFilePath optname desc =
  strOption
    $ long optname
        <> metavar "FILEPATH"
        <> help desc
        <> completer (bashCompleter "file")

data InitialFund = InitialFund
    { value       :: Word64
    , keyFile     :: FilePath
    , utxo        :: TxIn
    , addressFile :: FilePath
    }
    deriving Show

parseInitialFund :: Parser InitialFund
parseInitialFund
  = InitialFund
      <$> (option auto $ long "fund-value"
           <> help "Lovelace value of the initial fund"
           <> metavar "LOVELACE"
          )
      <*> Opt.strOption
         (  Opt.long "signing-key-file"
         <> Opt.metavar "FILE"
         <> Opt.help ("Input filepath of the signing key")
         <> Opt.completer (Opt.bashCompleter "file")
         )
      <*> pTxIn
      <*> (strOption   $ long "fund-addr"  <> help "address used for transactions" <> metavar "FILE")

pNetwork :: Parser NetworkId
pNetwork =
  pMainnet <|> fmap Testnet pTestnetMagic

pMainnet :: Parser NetworkId
pMainnet =
  Opt.flag' Mainnet
    (  Opt.long "mainnet"
    <> Opt.help "Use the mainnet magic id."
    )

pTestnetMagic :: Parser NetworkMagic
pTestnetMagic =
  NetworkMagic <$>
    Opt.option Opt.auto
      (  Opt.long "testnet-magic"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Specify a testnet magic id."
      )

pTxIn :: Parser TxIn
pTxIn =
  Opt.option (readerFromAttoParser parseTxIn)
    (  Opt.long "tx-in"
    <> Opt.metavar "TX-IN"
    <> Opt.help "The input transaction as TxId#TxIx where TxId is the transaction hash and TxIx is the index."
    )

readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
    Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . BSC.pack)
