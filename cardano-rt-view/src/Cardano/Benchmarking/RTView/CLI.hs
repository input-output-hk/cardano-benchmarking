module Cardano.Benchmarking.RTView.CLI
    ( RTViewParams (..)
    , parseRTViewParams
    ) where

import           Cardano.Prelude hiding ( option )
import           Prelude
                   ( String )
import           Options.Applicative
                   ( Parser
                   , auto, bashCompleter, completer, help
                   , long, metavar, option, strOption
                   )
import           Network.Socket
                   ( PortNumber )

-- | Type for CLI parameters required for the service.
data RTViewParams =
  RTViewParams
    FilePath
    FilePath
    PortNumber

parseRTViewParams :: Parser RTViewParams
parseRTViewParams =
  RTViewParams
    <$> parseFilePath
          "config"
          "file"
          "Configuration file for real-time view service"
    <*> parseFilePath
          "static"
          "directory"
          "Directory with static content"
    <*> parsePort
          "port"
          "The port number"

-- Aux parsers

parseFilePath
  :: String
  -> String
  -> String
  -> Parser FilePath
parseFilePath optname completion desc =
  strOption
    $ long optname
        <> metavar "FILEPATH"
        <> help desc
        <> completer (bashCompleter completion)

parsePort
  :: String
  -> String
  -> Parser PortNumber
parsePort optname desc =
    option ((fromIntegral :: Int -> PortNumber) <$> auto) (
          long optname
       <> metavar "PORT"
       <> help desc
    )

