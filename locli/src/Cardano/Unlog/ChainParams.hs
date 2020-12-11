module Cardano.Unlog.ChainParams (module Cardano.Unlog.ChainParams) where

import           Prelude

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Attoparsec.Time as Iso8601
import           Data.Text (pack)
import qualified Data.Time.Clock.POSIX as Time
import           Data.Time.Clock (UTCTime, NominalDiffTime)
import           Data.Word
import           Options.Applicative
import qualified Options.Applicative as Opt


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
