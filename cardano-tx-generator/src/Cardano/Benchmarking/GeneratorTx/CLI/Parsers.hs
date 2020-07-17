{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
module Cardano.Benchmarking.GeneratorTx.CLI.Parsers
  (module Cardano.Benchmarking.GeneratorTx.CLI.Parsers)
where

import           Cardano.Prelude hiding (option)
import           Prelude (String)
import           Options.Applicative
                    ( Parser
                    , auto, bashCompleter, completer, flag, help
                    , long, metavar, option, strOption
                    )
import qualified Control.Arrow as Arr
import           Network.Socket (PortNumber)

import           Cardano.Api.Typed
import           Ouroboros.Consensus.Block.Abstract(SlotNo(..))
import           Cardano.Config.Types
                    ( SigningKeyFile(..)
                    , SocketPath(..)
                    , NodeAddress(..)
                    , NodeHostAddress(..)
                    )


lastly :: Parser a -> Parser (Last a)
lastly = (Last <$>) . optional

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

parseFeePerTx :: String -> String -> Parser Lovelace
parseFeePerTx opt desc = Lovelace <$> parseIntegral opt desc

parseInitialTTL :: String -> String -> Parser SlotNo
parseInitialTTL opt desc = SlotNo <$> parseIntegral opt desc

parseSigningKeysFile :: String -> String -> Parser SigningKeyFile
parseSigningKeysFile opt desc = SigningKeyFile <$> parseFilePath opt desc

------------------------------------------------------------------

parseIntegral :: Integral a => String -> String -> Parser a
parseIntegral optname desc = option (fromInteger <$> auto)
  $ long optname <> metavar "INT" <> help desc

parseDouble :: String -> String -> Parser Double
parseDouble optname desc = option auto
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
