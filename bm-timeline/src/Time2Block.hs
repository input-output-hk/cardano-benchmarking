{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  (
    main
  )
where

import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Time.Clock (diffUTCTime)
import System.Environment (getArgs)
import System.IO (IOMode(..), withFile)

import Cardano.BM.Common
import Cardano.BM.Csv
import Cardano.BM.Stats
import qualified Cardano.BM.TxAdopted as TxAdopted
import qualified Cardano.BM.TxMempool as TxMempool


{- data types -}
type Milliseconds = Int

type KeyTxAdopted = TxId
type ValTxAdopted = TxAdopted.TxAdopted
type HMTxAdopted = HM.HashMap KeyTxAdopted ValTxAdopted


{- main procedure -}
main :: IO ()
main = do
    -- argument: base path to the directory containing the CSV files
    --           extracted from logs from all nodes
    basep <- parseArguments
    process basep

parseArguments :: IO FilePath
parseArguments = do
    parsed <- getArgs >>= return . parseArgs
    case parsed of
        Left m -> TLIO.putStrLn "call: bmtime2block <basepath>" >> error m
        Right res -> return res

  where
    parseArgs [] = Left "no arguments"
    parseArgs ( bp : []) = Right bp
    parseArgs _ = Left "too many arguments"

process :: FilePath -> IO ()
process basep = do
  (txadopted, txmempool) <- processCsv basep
  withFile "time2block.csv" WriteMode $ \h -> do
      let hm_txadopted = collect_txadopted txadopted
      let time2block = calc_time2block hm_txadopted txmempool
      -- TLIO.putStrLn . TL.pack . show $ length txmempool
      -- TLIO.putStrLn . TL.pack . show $ length txadopted
      -- TLIO.putStrLn . TL.pack . show $ length time2block
      let cdf = calc_cdf time2block
      TLIO.putStrLn . TL.pack . show $ cdf
      let boxplot = calc_boxplot time2block
      TLIO.putStrLn . TL.pack . show $ boxplot
      output_csv h $ named_columns [ (["milliseconds", "fraction"], pairs_in_columns(cdf))
                                   , (["min","q1","median","q3","max"], list_to_columns(boxplot))]
  return ()

collect_txadopted :: [TxAdopted.TxAdopted] -> HMTxAdopted
collect_txadopted ls =
    HM.fromList $ zip (map TxAdopted.txid ls) ls


calc_time2block :: HMTxAdopted -> [TxMempool.TxMempool] -> [Milliseconds]
calc_time2block hmtxadopted = mapMaybe calc_time2block'
  where
    calc_time2block' :: TxMempool.TxMempool -> Maybe Milliseconds
    calc_time2block' txmp = do
        adopted <- HM.lookup (TxMempool.txid txmp) hmtxadopted
        let tdiff = diffUTCTime (TxAdopted.timestamp adopted) (TxMempool.timestamp txmp)
        return $ round (tdiff * 1000.0)

processCsv :: FilePath -> IO ([TxAdopted.TxAdopted], [TxMempool.TxMempool])
processCsv basep = do
  d1 <- processTxAdopted basep
  d2 <- processTxMempool basep
  return (d1, d2)

processTxAdopted :: FilePath -> IO [TxAdopted.TxAdopted]
processTxAdopted basep = do
  csv <- TLIO.readFile $ basep ++ "/txadopted.csv"
  mapLines csv

processTxMempool :: FilePath -> IO [TxMempool.TxMempool]
processTxMempool basep = do
  csv <- TLIO.readFile $ basep ++ "/txmempool.csv"
  mapLines csv

mapLines :: Lineparser a => Text -> IO [a]
mapLines csv =
  return $ map parseline (TL.lines csv)