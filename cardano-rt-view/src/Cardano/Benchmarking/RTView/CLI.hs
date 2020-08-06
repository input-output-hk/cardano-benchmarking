module Cardano.Benchmarking.RTView.CLI
    ( RTViewParams (..)
    , parseRTViewParams
    ) where

import           Cardano.Prelude hiding (option)
import           Network.Socket (PortNumber)
import           Options.Applicative (Parser, auto, bashCompleter, completer, help, long, metavar,
                                      option, showDefault, strOption, value)
import           Prelude (String)

-- | Type for CLI parameters required for the service.
data RTViewParams
  = RTViewParams
      { rtvConfig             :: !FilePath
      , rtvStatic             :: !FilePath
      , rtvPort               :: !PortNumber
      , rtvNodeInfoLife       :: !Word64
      , rtvBlockchainInfoLife :: !Word64
      , rtvResourcesInfoLife  :: !Word64
      , rtvRTSInfoLife        :: !Word64
      }

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
    <*> parseDiffTime
          "node-info-life"
          "Lifetime of node info"
          5
    <*> parseDiffTime
          "blockchain-info-life"
          "Lifetime of blockchain info"
          35
    <*> parseDiffTime
          "resources-info-life"
          "Lifetime of resources info"
          35
    <*> parseDiffTime
          "rts-info-life"
          "Lifetime of GHC RTS info"
          45

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

parseDiffTime
  :: String
  -> String
  -> Int
  -> Parser Word64
parseDiffTime optname desc defaultTimeInSec =
  option (secToNanosec <$> auto) (
       long optname
    <> metavar "DIFFTIME"
    <> help desc
    <> value (secToNanosec defaultTimeInSec)
    <> showDefault
  )
 where
  secToNanosec :: Int -> Word64
  secToNanosec s = fromIntegral $ s * 1000000000
