{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | CLI command types
module Cardano.Unlog.Commands
  ( -- * CLI command types
    AnalysisCommand (..)
  , parseAnalysisCommands
  , renderAnalysisCommand

    -- * CLI flag types
  , ChainParams (..)
  , JsonLogfile (..)
  , JsonOutputFile (..)
  , TextOutputFile (..)
  ) where

import           Prelude

import qualified Data.Attoparsec.Time as Iso8601
import qualified Data.Attoparsec.Text as Atto
import           Data.Text (Text, pack)
import           Data.Time.Clock (UTCTime, NominalDiffTime)
import qualified Data.Time.Clock.POSIX as Time
import           Data.Word

import           Options.Applicative
import qualified Options.Applicative as Opt

import           Cardano.Unlog.LogObject

--
-- Analysis CLI command data types
--

-- | All the CLI subcommands under \"analysis\".
--
data AnalysisCommand
  = LeadershipChecks ChainParams (Maybe JsonOutputFile) (Maybe TextOutputFile) (Maybe JsonOutputFile) [JsonLogfile]
  | SubstringKeys
  deriving (Show)

renderAnalysisCommand :: AnalysisCommand -> Text
renderAnalysisCommand sc =
  case sc of
    LeadershipChecks {} -> "analyse leadership"
    SubstringKeys {}    -> "analyse substring-keys"

parseAnalysisCommands :: Parser AnalysisCommand
parseAnalysisCommands =
  Opt.subparser $
    mconcat
      [ Opt.command "leadership"
          (Opt.info (LeadershipChecks
                       <$> pChainParams
                       <*> optional
                           (argJsonOutputFile "dump-leaderships"
                              "Dump extracted slot leadership summaries, as a side-effect of log analysis")
                       <*> optional
                           (argTextOutputFile "dump-pretty-timeline"
                              "Dump pretty timeline of extracted slot leadership summaries, as a side-effect of log analysis")
                       <*> optional
                           (argJsonOutputFile "analysis-output"
                              "Write analysis JSON to this file, if specified -- otherwise print to stdout.")
                       <*> some argJsonLogfile) $
            Opt.progDesc "Analyse leadership checks")
      , Opt.command "substring-keys"
          (Opt.info (pure SubstringKeys) $
            Opt.progDesc "Dump substrings that narrow logs to relevant subset")
      ]

--
-- Analysis CLI flag/option data types
--

argJsonLogfile :: Parser JsonLogfile
argJsonLogfile =
  JsonLogfile <$>
    Opt.argument Opt.str (Opt.metavar "JSON-LOGFILE")

argJsonOutputFile :: String -> String -> Parser JsonOutputFile
argJsonOutputFile optname desc =
  fmap JsonOutputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "JSON-OUTFILE"
      <> help desc

argTextOutputFile :: String -> String -> Parser TextOutputFile
argTextOutputFile optname desc =
  fmap TextOutputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "TEXT-OUTFILE"
      <> help desc

optUTCTime :: String -> String -> UTCTime -> Parser UTCTime
optUTCTime optname desc def =
  Opt.option (readerFromAttoParser Iso8601.utcTime)
    $ long optname
    <> metavar "ISO8601-TIME"
    <> help desc
    <> value def

optDuration :: String -> String -> NominalDiffTime -> Parser NominalDiffTime
optDuration optname desc def=
  Opt.option ((realToFrac :: Double -> NominalDiffTime) <$> Opt.auto)
    $ long optname
    <> metavar "SEC"
    <> help desc
    <> value def

optWord :: String -> String -> Word64 -> Parser Word64
optWord optname desc def =
  Opt.option auto
    $ long optname
    <> metavar "INT"
    <> help desc
    <> value def

-- Stolen from: cardano-cli/src/Cardano/CLI/Shelley/Parsers.hs
readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
    Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . pack)

data ChainParams
  = ChainParams
    { cpSystemStart :: !UTCTime
    , cpSlotLength  :: !NominalDiffTime
    , cpEpochSlots  :: !Word64
    }
  deriving Show

pChainParams :: Parser ChainParams
pChainParams =
  ChainParams
    <$> (optUTCTime  "system-start"
                     "Cluster system start time."
                    (cpSystemStart defaultChainParams))
    <*> (optDuration "slot-length"
                     "Slot length, in seconds (Double)."
                    (cpSlotLength defaultChainParams))
    <*> (optWord     "epoch-slots"
                     "Epoch length, in slots (Word)."
                    (cpEpochSlots defaultChainParams))

defaultChainParams :: ChainParams
defaultChainParams =
  ChainParams
  { cpSystemStart = Time.posixSecondsToUTCTime $ realToFrac (0 :: Int)
  , cpSlotLength  = realToFrac (1 :: Int)
  , cpEpochSlots  = 2200 -- 10 * k(=10) / f(=0.05) + 100
  }
