{-# LANGUAGE OverloadedStrings #-}

module Main
  (
    main
  )
where

import Control.Monad (forM, forM_)
import qualified Data.HashMap.Strict as HM
import Data.List (sort, sortOn)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Time.Clock (diffUTCTime)
import System.Environment (getArgs)
import System.IO (Handle, IOMode(..), withFile)
import Text.Read (readMaybe)

import Cardano.BM.Common
import qualified Cardano.BM.AddToChain as AddToChain
import qualified Cardano.BM.Adopted as Adopted
import qualified Cardano.BM.Leader as Leader


{- data types -}
type KeyLeader = SlotNum
type ValLeader = (NodeId, Timestamp)
type HMLeader = HM.HashMap KeyLeader ValLeader

type KeyAdopted = SlotNum
type ValAdopted = (NodeId, Timestamp, Int)
type HMAdopted = HM.HashMap KeyAdopted ValAdopted

type KeyChain = (NodeId, SlotNum)
type ValChain = Timestamp
type HMChain = HM.HashMap KeyChain ValChain


{- main procedure -}
main :: IO ()
main = do
    (n, basep) <- parseArguments
    let nodes = mknodes n
    processCsv basep nodes

parseArguments :: IO (Int, FilePath)
parseArguments = do
    parsed <- getArgs >>= return . parseArgs
    case parsed of
        Left m -> TLIO.putStrLn "call: reconstruct-timeline <#nodes> <basepath>" >> error m
        Right res -> return res

  where
    parseArgs [] = Left "no arguments"
    parseArgs (_a : []) = Left "not enough arguments"
    parseArgs (_a : _b : []) = Left "not enough arguments"
    parseArgs ( _ : n : bp : []) =
        case readMaybe n of
            Nothing -> Left $ "can't parse: " ++ n
            Just v -> if v > 0
              then Right (v, bp)
              else Left $ "wrong value: " ++ n
    parseArgs _ = Left "too many arguments"

mknodes :: Int -> [NodeId]
mknodes n = [0 .. n-1]

processCsv :: FilePath -> [NodeId] -> IO ()
processCsv basep nodes = do
  let snodes = map (TL.pack . show) nodes
  parsed <- forM snodes $ processNodeCsv basep
  withFile "timeline.csv" WriteMode $ \h -> do
      -- forM_ parsed $  TLIO.putStrLn . TL.pack . show
      let hm_adopted0 = HM.empty :: HMAdopted
      let hm_chain0 = HM.empty :: HMChain
      let hm_leader0 = HM.empty :: HMLeader
      let (hm_chain, hm_adopted, hm_leader) = collect parsed (hm_chain0,hm_adopted0,hm_leader0)
      -- TLIO.putStrLn . TL.pack . show $ hm_chain
      -- TLIO.putStrLn . TL.pack . show $ hm_adopted
      -- TLIO.putStrLn . TL.pack . show $ hm_leader
      -- TLIO.putStrLn . TL.pack . show $ sort $ HM.keys hm_leader
      reconstruct h nodes (sort $ HM.keys hm_leader) (hm_chain,hm_adopted,hm_leader)
  return ()

reconstruct :: Handle -> [NodeId] -> [SlotNum] -> (HMChain, HMAdopted, HMLeader) -> IO ()
reconstruct _ _ [] _ = pure ()
reconstruct h nodes (sn : sns) (hmch, hmad, hmld) = do
  TLIO.putStr $ TL.pack (show sn) <> " SLOT            "
  forM_ nodes $ \n -> TLIO.putStr $ "node " <> TL.pack (show n) <> "                "
  TLIO.putStrLn ""
  leader <- reconstruct_leader sn (HM.lookup sn hmld)
  adopted <- reconstruct_adopt h sn leader (HM.lookup sn hmad)
  let chaints = nodes `zip` map (\n -> HM.lookup (n,sn) hmch) nodes
  let ochaints = sortOn snd chaints
  forM_ ochaints $ \(n,ts) -> reconstruct_chain h sn adopted n ts
  TLIO.putStrLn ""
  reconstruct h nodes sns (hmch, hmad, hmld)

reconstruct_leader :: SlotNum -> Maybe ValLeader -> IO ValLeader
reconstruct_leader _ Nothing = pure (-1, time0)
reconstruct_leader _ (Just v@(n,ts)) = do
  TLIO.putStrLn $ TL.pack (show n) <> " lead     " <> (indent n) <> formatTS ts
  return v
reconstruct_adopt:: Handle -> SlotNum -> ValLeader -> Maybe ValAdopted -> IO ValAdopted
reconstruct_adopt _ _ _ Nothing = pure (-1, time0, 0)
reconstruct_adopt h sn (nl, tsl) (Just v@(n,ts,numtx)) = do
  let nsrc = TL.pack (show nl)
      ntgt = TL.pack (show n)
      ntx = TL.pack (show numtx)
      slot = TL.pack (show sn)
      tdiff = diffUTCTime ts tsl
      tdiffms = round (tdiff * 1000.0) :: Int
  TLIO.putStrLn $ ntgt <> " adopt    " <> (indent n) <> formatTS ts
  TLIO.putStrLn $ nsrc <> "->" <> ntgt <> " creation t   " <> (indent n) <> (TL.pack . show) tdiff
  TLIO.hPutStrLn h $ TL.intercalate "," [slot, nsrc, ntgt, "adopted", (TL.pack . show) tdiffms, ntx]
  return v
reconstruct_chain :: Handle -> SlotNum -> ValAdopted -> NodeId -> Maybe ValChain -> IO ()
reconstruct_chain _ _ _ _ Nothing = pure ()
reconstruct_chain h sn (na, tsa, _ntx) n (Just ts) = do
  let nsrc = TL.pack (show na)
      ntgt = TL.pack (show n)
      slot = TL.pack (show sn)
      tdiff = diffUTCTime ts tsa
      tdiffms = round (tdiff * 1000.0) :: Int
  TLIO.putStrLn $ ntgt <> " chain    " <> (indent n) <> formatTS ts
  TLIO.putStrLn $ nsrc <> "->" <> ntgt <> " diffusion t  " <> (indent n) <> (TL.pack . show) tdiff
  TLIO.hPutStrLn h $ TL.intercalate "," [slot, nsrc, ntgt, "diffusion", (TL.pack . show) tdiffms]

indent :: NodeId -> Text
indent 0 = ""
indent 1 = "                      "
indent n = indent 1 <> indent (n - 1)

collect :: [([AddToChain.AddToChain], [Adopted.Adopted], [Leader.Leader])]
        -> (HMChain, HMAdopted, HMLeader)
        -> (HMChain, HMAdopted, HMLeader)
collect [] triple = triple
collect ((cs,as,ls) : ts) (hmch,hmad,hmld) =
    collect ts (collect_chain cs hmch, collect_adopt as hmad, collect_leader ls hmld)

collect_chain :: [AddToChain.AddToChain] -> HMChain -> HMChain
collect_chain [] hm = hm
collect_chain ((AddToChain.AddToChain n sn ts _m) : cs) hm =
    collect_chain cs $
      case HM.lookup (n,sn) hm of
          Just _ -> hm   -- ignore
          Nothing -> HM.insert (n,sn) ts hm

collect_adopt :: [Adopted.Adopted] -> HMAdopted -> HMAdopted
collect_adopt [] hm = hm
collect_adopt ((Adopted.Adopted n sn ts _m _bh ntx) : as) hm =
    collect_adopt as $
      case HM.lookup sn hm of
          Just _ -> hm -- error $ "Adopted already defined! " ++ (show sn)
          Nothing -> HM.insert sn (n,ts,ntx) hm

collect_leader :: [Leader.Leader] -> HMLeader -> HMLeader
collect_leader [] hm = hm
collect_leader ((Leader.Leader n sn ts _m) : ls) hm =
    collect_leader ls $
      case HM.lookup sn hm of
          Just _ -> hm -- error $ "Leader already defined! " ++ (show sn)
          Nothing -> HM.insert sn (n,ts) hm

processNodeCsv :: FilePath -> Text -> IO ([AddToChain.AddToChain], [Adopted.Adopted], [Leader.Leader])
processNodeCsv basep node = do
  d1 <- processNodeChain basep node
  d2 <- processNodeAdopted basep node
  d3 <- processNodeLeader basep node
  return (d1, d2, d3)

processNodeChain :: FilePath -> Text -> IO [AddToChain.AddToChain]
processNodeChain basep node = do
  csv <- TLIO.readFile $ basep ++ "/addtochain-" ++ TL.unpack node ++ ".csv"
  mapLines csv

processNodeAdopted :: FilePath -> Text -> IO [Adopted.Adopted]
processNodeAdopted basep node = do
  csv <- TLIO.readFile $ basep ++ "/adopted-" ++ TL.unpack node ++ ".csv"
  mapLines csv

processNodeLeader :: FilePath -> Text -> IO [Leader.Leader]
processNodeLeader basep node = do
  csv <- TLIO.readFile $ basep ++ "/leader-" ++ TL.unpack node ++ ".csv"
  mapLines csv

mapLines :: Lineparser a => Text -> IO [a]
mapLines csv =
  return $ map parseline (TL.lines csv)