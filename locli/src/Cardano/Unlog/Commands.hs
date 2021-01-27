{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | CLI command types
module Cardano.Unlog.Commands (module Cardano.Unlog.Commands) where

import           Prelude

import           Data.Text (Text)

import           Options.Applicative
import qualified Options.Applicative as Opt

import           Cardano.Unlog.LogObject hiding (Text)

--
-- Analysis CLI command data types
--

-- | All the CLI subcommands under \"analysis\".
--
data AnalysisCommand
  = LeadershipChecks JsonGenesisFile JsonRunMetafile (Maybe JsonOutputFile) (Maybe JsonOutputFile) (Maybe TextOutputFile) (Maybe TextOutputFile) (Maybe TextOutputFile) (Maybe EpsOutputFile) (Maybe JsonOutputFile) [JsonLogfile]
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
                       <$> argJsonGenesisFile "genesis"
                              "Genesis file of the run"
                       <*> argJsonRunMetafile "run-metafile"
                              "The meta.json file from the benchmark run"
                       <*> optional
                           (argJsonOutputFile "dump-logobjects"
                              "Dump the entire input LogObject stream")
                       <*> optional
                           (argJsonOutputFile "dump-leaderships"
                              "Dump extracted slot leadership summaries, as a side-effect of log analysis")
                       <*> optional
                           (argTextOutputFile "pretty-timeline"
                              "Dump pretty timeline of extracted slot leadership summaries, as a side-effect of log analysis")
                       <*> optional
                           (argTextOutputFile "export-timeline"
                              "Dump CSV of the timeline")
                       <*> optional
                           (argTextOutputFile "export-stats"
                              "Dump CSV of the timeline statistics")
                       <*> optional
                           (argEpsOutputFile "cpu-spans-histogram"
                              "Write an EPS file with the CPU spans histogram")
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

argJsonGenesisFile :: String -> String -> Parser JsonGenesisFile
argJsonGenesisFile optname desc =
  fmap JsonGenesisFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "JSON-GENESIS-FILE"
      <> help desc

argJsonRunMetafile :: String -> String -> Parser JsonRunMetafile
argJsonRunMetafile optname desc =
  fmap JsonRunMetafile $
    Opt.option Opt.str
      $ long optname
      <> metavar "JSON-RUN-METAFILE"
      <> help desc

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

argEpsOutputFile :: String -> String -> Parser EpsOutputFile
argEpsOutputFile optname desc =
  fmap EpsOutputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "EPS-OUTFILE"
      <> help desc
