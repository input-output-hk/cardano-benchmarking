module Cardano.Benchmarking.RTView.CLI
    ( RTViewParams (..)
    , parseRTViewParams
    ) where

import           Cardano.Prelude
import           Prelude
                   ( String )
import           Options.Applicative
                   ( Parser
                   , bashCompleter, completer, help
                   , long, metavar, strOption
                   )

-- | Type for all CLI parameters required for the service.
data RTViewParams =
  RTViewParams FilePath

parseRTViewParams :: Parser RTViewParams
parseRTViewParams =
  RTViewParams
    <$> parseFilePath
          "config"
          "Configuration file for real-time view service"

-- Aux parsers

parseFilePath :: String -> String -> Parser FilePath
parseFilePath optname desc =
  strOption
    $ long optname
        <> metavar "FILEPATH"
        <> help desc
        <> completer (bashCompleter "file")
