{-# LANGUAGE OverloadedStrings #-}

module Main
  (
    main
  )
where

import Control.Monad (forM)
import qualified Data.HashMap.Strict as HM
import Data.List (sort, sortOn)
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Time.Clock (diffUTCTime)
import System.Environment (getArgs)
import System.IO (IOMode(..), withFile)
import Text.Read (readMaybe)

import Cardano.BM.Common
import Cardano.BM.Csv
import Cardano.BM.Stats
import qualified Cardano.BM.AddToChain as AddToChain
import qualified Cardano.BM.Adopted as Adopted
import qualified Cardano.BM.Leader as Leader


{- data types -}
type Milliseconds = Int
type AdoptedTimeTuple = (SlotNum, NodeId, Milliseconds, Int, Int)
type DiffusionTimeTuple = (SlotNum, NodeId, NodeId, Milliseconds, Int, Int)

type KeyLeader = SlotNum
type ValLeader = (NodeId, Timestamp)
type HMLeader = HM.HashMap KeyLeader ValLeader

type KeyAdopted = SlotNum
type ValAdopted = (NodeId, Timestamp, Int, Int)
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
        Left m -> TLIO.putStrLn "call: bmadoption <#nodes> <basepath>" >> error m
        Right res -> return res

  where
    parseArgs [] = Left "no arguments"
    parseArgs (_a : []) = Left "not enough arguments"
    parseArgs ( n : bp : []) =
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
  parsed <- forM snodes $ loadNodeCsv basep
  let hm_adopted0 = HM.empty :: HMAdopted
  let hm_chain0 = HM.empty :: HMChain
  let hm_leader0 = HM.empty :: HMLeader
  let (hm_chain, hm_adopted, hm_leader) = collect parsed (hm_chain0,hm_adopted0,hm_leader0)
  (tadopted0, tdiffusion0) <- reconstruct nodes (sort $ HM.keys hm_leader) (hm_chain,hm_adopted,hm_leader) [] []
  -- sort by slot number; filter out entries with invalid slot number
  let tadopted = sortOn (\(sn, _, _, _, _) -> sn) $ filter (\(sn, _, _, _, _) -> sn > 0) tadopted0
  let tdiffusion = sortOn (\(sn, _, _, _, _, _) -> sn) tdiffusion0
  -- calculate time between blocks (adopted)
  let tbetween n = calculate_time_between hm_adopted n
  let cdf_tbetween = foldl (\acc n -> pairs_in_columns(calc_cdf (tbetween n)) : acc) [] $ reverse nodes
  let cdf_tb_headers = foldl (\acc n -> ["time between blocks (node " <> (T.pack . show) n <> ")","fraction node " <> (T.pack . show) n] : acc)
                             [] $ reverse nodes
  -- compute boxplot of block adoption time; prepend with source node id
  let bp_adopted1 n = [textify n] <> (map textify $ calc_boxplot $ map (\(_, _, t, _, _) -> t) $ filter (\(_, src, _, _, _) -> src == n) tadopted)
  let bp_adopted = foldl (\acc n -> bp_adopted1 n : acc) [] $ reverse nodes
  -- compute boxplot of block diffusion time; prepend with source and target node ids
  let bp_diffusion2 n1 n2 = [textify n1, textify n2] <> (map textify $ calc_boxplot $ map (\(_, _, _, t, _, _) -> t) $ filter (\(_, src, tgt, _, _, _) -> src == n1 && tgt == n2) tdiffusion)
  let bp_diffusion1 n = foldl (\acc n2 -> bp_diffusion2 n n2 : acc) [] $ reverse nodes
  let bp_diffusion = foldl (\acc n -> bp_diffusion1 n : acc) [] $ reverse nodes
  let bp_diff_headers = map (const ["src node", "tgt node", "min","q1","median","q3","max"]) nodes
  withFile "adoption.csv" WriteMode $ \h ->
      output_csv h $ named_columns $ [ (["slot", "node id", "tadopted", "num tx", "block size"], tuple5_in_columns(tadopted))
                                     , (["node id", "min","q1","median","q3","max"], bp_adopted) ]
                                    <> zip cdf_tb_headers cdf_tbetween
  withFile "diffusion.csv" WriteMode $ \h ->
      output_csv h $ named_columns $ [ (["slot", "src node", "tgt node", "tdiffusion", "num tx", "block size"], tuple6_in_columns(tdiffusion)) ]
                                    <> zip bp_diff_headers bp_diffusion

  return ()


calculate_time_between :: HMAdopted -> NodeId -> [Milliseconds]
calculate_time_between hm n =
  let adls = sortOn fst $ filter (\(_,(node,_,_,_)) -> node == n) $ HM.toList hm
  in map timedifference $ zip adls (tail adls)
 where
   timedifference :: ((SlotNum,ValAdopted),(SlotNum,ValAdopted)) -> Milliseconds
   timedifference ((_sn1,(_,ts1,_,_)), (_sn2,(_,ts2,_,_))) =
     round $ 1000.0 * diffUTCTime ts2 ts1

{-
setAt :: Int -> a -> [a] -> [a]
setAt _ _val [] = []
setAt 0 val (_v : vs) = val : vs
setAt idx val (v : vs) = v : setAt (idx - 1) val vs
-}

reconstruct :: [NodeId] -> [SlotNum] -> (HMChain, HMAdopted, HMLeader)
            -> [AdoptedTimeTuple] -> [DiffusionTimeTuple]
            -> IO ([AdoptedTimeTuple], [DiffusionTimeTuple])
reconstruct _ [] _ acc1 acc2 = pure (acc1, acc2)
reconstruct nodes (sn : sns) (hmch, hmad, hmld) acc1 acc2 = do
  leader <- reconstruct_leader sn (HM.lookup sn hmld)
  recadopt <- reconstruct_adopt sn leader (HM.lookup sn hmad)
  (tdiffadopted, tdiffusion0) <- case recadopt of
    Just (tdiffadopted, adopted@(nid,_,ntx,bsz)) -> do
        let chaints = nodes `zip` map (\n -> HM.lookup (n,sn) hmch) nodes
        let ochaints = sortOn snd chaints
        tdiffusion <- forM ochaints $ \(n,ts) -> reconstruct_chain sn adopted n ts
        pure ((sn,nid,tdiffadopted,ntx,bsz), tdiffusion)
    Nothing -> pure ((0, -1, -1, 0, 0), [])
  let tdiffusion = catMaybes tdiffusion0
  reconstruct nodes sns (hmch, hmad, hmld) (tdiffadopted : acc1) (tdiffusion ++ acc2)

reconstruct_leader :: SlotNum -> Maybe ValLeader -> IO ValLeader
reconstruct_leader _ Nothing = pure (-1, time0)
reconstruct_leader _ (Just v) = return v

reconstruct_adopt :: SlotNum -> ValLeader -> Maybe ValAdopted
                  -> IO (Maybe (Milliseconds, ValAdopted))
reconstruct_adopt _ _ Nothing = return Nothing
reconstruct_adopt _sn (_nl, tsl) (Just v@(_n,ts,_numtx,_blocksize)) =
  let tdiff = diffUTCTime ts tsl
      tdiffms = round (tdiff * 1000.0) :: Milliseconds
  in return $ Just (tdiffms, v)

reconstruct_chain :: SlotNum -> ValAdopted -> NodeId -> Maybe ValChain
                  -> IO (Maybe DiffusionTimeTuple)
reconstruct_chain _ _ _ Nothing = pure Nothing
reconstruct_chain sn (na, tsa, ntx, bsz) n (Just ts) = do
  let tdiff = diffUTCTime ts tsa
      tdiffms = round (tdiff * 1000.0) :: Milliseconds
  return $ Just (sn, na, n, tdiffms, ntx, bsz)

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
collect_adopt ((Adopted.Adopted n sn ts _m _bh ntx bsz) : as) hm =
    collect_adopt as $
      case HM.lookup sn hm of
          Just _ -> hm -- error $ "Adopted already defined! " ++ (show sn)
          Nothing -> HM.insert sn (n,ts,ntx,bsz) hm

collect_leader :: [Leader.Leader] -> HMLeader -> HMLeader
collect_leader [] hm = hm
collect_leader ((Leader.Leader n sn ts _m) : ls) hm =
    collect_leader ls $
      case HM.lookup sn hm of
          Just _ -> hm -- error $ "Leader already defined! " ++ (show sn)
          Nothing -> HM.insert sn (n,ts) hm

loadNodeCsv :: FilePath -> Text -> IO ([AddToChain.AddToChain], [Adopted.Adopted], [Leader.Leader])
loadNodeCsv basep node = do
  d1 <- loadNodeChain basep node
  d2 <- loadNodeAdopted basep node
  d3 <- loadNodeLeader basep node
  return (d1, d2, d3)

loadNodeChain :: FilePath -> Text -> IO [AddToChain.AddToChain]
loadNodeChain basep node = do
  csv <- TLIO.readFile $ basep ++ "/addtochain-" ++ TL.unpack node ++ ".csv"
  mapLines csv

loadNodeAdopted :: FilePath -> Text -> IO [Adopted.Adopted]
loadNodeAdopted basep node = do
  csv <- TLIO.readFile $ basep ++ "/adopted-" ++ TL.unpack node ++ ".csv"
  mapLines csv

loadNodeLeader :: FilePath -> Text -> IO [Leader.Leader]
loadNodeLeader basep node = do
  csv <- TLIO.readFile $ basep ++ "/leader-" ++ TL.unpack node ++ ".csv"
  mapLines csv

mapLines :: Lineparser a => Text -> IO [a]
mapLines csv =
  return $ map parseline (TL.lines csv)