{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  (
    main
  )
where

import           Control.Monad (foldM)
import           Data.List (sortOn)
import qualified Data.Text as T
import           Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Time.Clock (diffUTCTime)
import           System.Environment (getArgs)
import           System.IO (IOMode (..), withFile)
import           Text.Read (readMaybe)

import           Cardano.BM.Common
import qualified Cardano.BM.CPUticks as CPUticks
import           Cardano.BM.Csv
import qualified Cardano.BM.MemResident as MemResident


{- main procedure -}
main :: IO ()
main = do
    (n, basep) <- parseArguments
    let nodes = mknodes n
    process basep nodes

mknodes :: Int -> [NodeId]
mknodes n = [0 .. n-1]

parseArguments :: IO (Int, FilePath)
parseArguments = do
    parsed <- getArgs >>= return . parseArgs
    case parsed of
        Left m -> TLIO.putStrLn "call: bmresources <basepath>" >> error m
        Right res -> return res

  where
    parseArgs [] = Left "no arguments"
    parseArgs (_a : []) = Left "not enough arguments"
    parseArgs (n : bp : []) =
        case readMaybe n of
            Nothing -> Left $ "can't parse: " ++ n
            Just v -> if v > 0
              then Right (v, bp)
              else Left $ "wrong value: " ++ n
    parseArgs _ = Left "too many arguments"

process :: FilePath -> [NodeId] -> IO ()
process basep nodes = do
  cputicks0 <- foldM (loadCPUticks basep) [] nodes
  memusage0 <- foldM (loadMemory basep) [] nodes
  withFile "resources.csv" WriteMode $ \h -> do
      let memusage = sortOn fst $ reverse $ map (\(t0,d) -> (TL.toStrict t0, d)) $ processMemory (length nodes) memusage0
      let cpuusage = sortOn fst $ reverse $ map (\(t0,d) -> (TL.toStrict t0, d)) $ processCPUticks (length nodes) cputicks0
      -- TLIO.putStrLn . TL.pack . show $ take 20 cpuusage
      -- TLIO.putStrLn . TL.pack . show $ take 20 memusage
      let header nm = "timestamp" : (map (\nid -> nm <> " " <> T.pack (show nid)) nodes)
      output_csv h $ named_columns [ (header "cpu", timestamp_with_list(cpuusage))
                                   , (header "mem", timestamp_with_list(memusage)) ]
  return ()

processMemory :: Int -> [MemResident.MemResident] -> [(Text, [Double])]
processMemory _nnodes [] = []
processMemory nnodes ticks = process_memory' ticks [(pack "",replicate nnodes 0.0)]
  where
    process_memory' :: [MemResident.MemResident] -> [(Text, [Double])] -> [(Text, [Double])]
    process_memory' [] acc = acc
    process_memory' (m : ms) acc =
      let ln = calc_memusage m
      in
      process_memory' ms (ln : acc)
    calc_memusage :: MemResident.MemResident -> (Text, [Double])
    calc_memusage mem =
      let nodeid = MemResident.node mem
          timestamp = MemResident.timestamp mem
          newmem :: Double = fromIntegral $ MemResident.memresident mem
          undefs = replicate nnodes (-1.0)
      in (formatTS timestamp
         , setAt nodeid
                 (newmem / 1000000.0) -- mem usage [MB]
                 undefs
         )

processCPUticks :: Int -> [CPUticks.CPUticks] -> [(Text, [Double])]
processCPUticks _nnodes [] = []
processCPUticks nnodes ticks = process_cputicks' ticks (replicate nnodes Nothing) [(pack "",replicate nnodes 0.0)]
  where
    process_cputicks' :: [CPUticks.CPUticks] -> [Maybe CPUticks.CPUticks] -> [(Text, [Double])] -> [(Text, [Double])]
    process_cputicks' [] _ acc = acc
    process_cputicks' (t : ts) lastticks acc =
      let (ln, lastticks') = calc_cpuusage t lastticks
      in
      process_cputicks' ts lastticks' (ln : acc)
    calc_cpuusage :: CPUticks.CPUticks -> [Maybe CPUticks.CPUticks] -> ((Text, [Double]), [Maybe CPUticks.CPUticks])
    calc_cpuusage cputicks lastticks =
      let nodeid = CPUticks.node cputicks
          timestamp = CPUticks.timestamp cputicks
          newticks = CPUticks.ticks cputicks
          undefs = replicate nnodes (-1.0)
      in case lastticks !! nodeid of
        Nothing  -> ( (formatTS timestamp
                      , setAt nodeid
                          0.0  -- usage [%]
                          undefs )
                    , setAt nodeid (Just cputicks) lastticks )
        Just cpu -> let difftime = realToFrac $ 1000.0 * diffUTCTime timestamp (CPUticks.timestamp cpu)
                        deltaticks = fromIntegral $ newticks - CPUticks.ticks cpu
                    in
                    ( (formatTS timestamp
                      , setAt nodeid
                          (deltaticks / difftime * 1000.0)  -- usage [%] since last
                          undefs )
                    , setAt nodeid (Just cputicks) lastticks )

setAt :: Int -> a -> [a] -> [a]
setAt _ _val []        = []
setAt 0 val (_v : vs)  = val : vs
setAt idx val (v : vs) = v : setAt (idx - 1) val vs

loadCPUticks :: FilePath -> [CPUticks.CPUticks] -> NodeId -> IO [CPUticks.CPUticks]
loadCPUticks basep acc nodeid = do
  csv <- TLIO.readFile $ basep ++ "/cpu-" ++ show nodeid ++ ".csv"
  ln <- mapLines csv
  return $ acc ++ map (\mem -> mem{CPUticks.node = nodeid}) ln

loadMemory :: FilePath -> [MemResident.MemResident] -> NodeId -> IO [MemResident.MemResident]
loadMemory basep acc nodeid = do
  csv <- TLIO.readFile $ basep ++ "/mem-" ++ show nodeid ++ ".csv"
  ln <- mapLines csv
  return $ acc ++ map (\mem -> mem{MemResident.node = nodeid}) ln

mapLines :: Lineparser a => Text -> IO [a]
mapLines csv =
  return $ map parseline (TL.lines csv)
