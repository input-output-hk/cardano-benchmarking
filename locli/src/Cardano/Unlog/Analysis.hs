{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Unlog.Analysis
  ( AnalysisCmdError
  , renderAnalysisCmdError
  , runAnalysisCommand
  ) where

import           Prelude (error)
import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import qualified Data.Aeson as Aeson
import           Data.Aeson (ToJSON(..))
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq(..))
import qualified Data.Text as Text
import qualified Data.Vector as V

import           Data.Time.Clock (UTCTime, NominalDiffTime)
import qualified Data.Time.Clock as Time
import           Text.Printf

import           Data.Accum
import           Data.Distribution
import           Cardano.Unlog.Commands
import           Cardano.Unlog.LogObject
import           Cardano.Unlog.Resources


data AnalysisCmdError
  = AnalysisCmdError !Text
  deriving Show

renderAnalysisCmdError :: AnalysisCommand -> AnalysisCmdError -> Text
renderAnalysisCmdError cmd err =
  case err of
    AnalysisCmdError err' ->
       renderError cmd identity err'
 where
   renderError :: AnalysisCommand -> (a -> Text) -> a -> Text
   renderError cmd' renderer cmdErr =
      mconcat [ "Analysis command failed: "
              , renderAnalysisCommand cmd'
              , "  Error: "
              , renderer cmdErr
              ]

--
-- CLI shelley command dispatch
--

runAnalysisCommand :: AnalysisCommand -> ExceptT AnalysisCmdError IO ()
runAnalysisCommand (LeadershipChecks startTime mJDumpFile mTDumpFile logfiles) =
  firstExceptT AnalysisCmdError $
    runLeadershipCheckCmd startTime mJDumpFile mTDumpFile logfiles
runAnalysisCommand SubstringKeys =
  liftIO $ mapM_ putStrLn logObjectStreamInterpreterKeys

runLeadershipCheckCmd ::
     ChainParams
  -> Maybe JsonOutputFile
  -> Maybe TextOutputFile
  -> [JsonLogfile]
  -> ExceptT Text IO ()
runLeadershipCheckCmd ChainParams{..}
 mLeadershipsDumpFile mPrettyDumpFile logfiles = do
  objs :: [LogObject] <- liftIO $
    concat <$> mapM readLogObjectStream logfiles
  -- liftIO $ withFile "lodump.json" WriteMode $ \hnd ->
  --   forM_ objs $ LBS.hPutStrLn hnd . Aeson.encode
  leads <- computeLeadershipStats objs
  liftIO $ do
    case mLeadershipsDumpFile of
      Nothing -> pure ()
      Just (JsonOutputFile f) ->
        withFile f WriteMode $ \hnd ->
          forM_ leads $ LBS.hPutStrLn hnd . Aeson.encode
    case mPrettyDumpFile of
      Nothing -> pure ()
      Just (TextOutputFile f) ->
        withFile f WriteMode $ \hnd ->
          forM_ (zip (toList leads) [(0 :: Int)..]) $ \(l, i) -> do
            when (i `mod` 20 == 0) $
              hPutStrLn hnd leadershipHeader
            hPutStrLn hnd $ toLeadershipLine l
    LBS.putStrLn $ Aeson.encode $ analysisSummary leads
 where
   slotStart :: Word64 -> UTCTime
   slotStart = flip Time.addUTCTime cpSystemStart . (* cpSlotLength) . fromIntegral

   computeLeadershipStats :: [LogObject] -> ExceptT Text IO (Seq SlotStats)
   computeLeadershipStats =
      fmap (Seq.reverse . Seq.fromList . aLeads) . foldlM go zeroAnalysis

   updateChecks :: UTCTime -> SlotStats -> SlotStats
   updateChecks now sl@SlotStats{..} =
     sl { slCountChecks = slCountChecks + 1
        , slSpan  = now `Time.diffUTCTime` slStart
        }

   updateLeads :: UTCTime -> SlotStats -> SlotStats
   updateLeads now sl@SlotStats{..} =
     sl { slCountLeads = slCountLeads + 1
        , slSpan  = now `Time.diffUTCTime` slStart
        }

   go :: Analysis -> LogObject -> ExceptT Text IO Analysis
   go a@Analysis{aLeads=cur:rSLs, ..} = \case
     lo@LogObject{loAt, loBody=LOTraceStartLeadershipCheck slot _} ->
       if slSlot cur > slot
       then pure $
            a { aLeads = cur
                { slOrderViol = slOrderViol cur + 1
                } : case (slSlot cur - slot, rSLs) of
                      -- Limited back-patching:
                      (1, p1:rest)       ->       updateChecks loAt p1:rest
                      (2, p1:p2:rest)    ->    p1:updateChecks loAt p2:rest
                      (3, p1:p2:p3:rest) -> p1:p2:updateChecks loAt p3:rest
                      _ -> rSLs -- Give up.
              }
       else if slSlot cur == slot
       then pure
            a { aLeads = updateChecks loAt cur : rSLs
              }
       else if slot - slSlot cur > 1
       then let gap = slot - slSlot cur - 1
                gapStartSlot = slSlot cur + 1 in
            pure $ updateOnNewSlot lo $ -- We have a slot check gap to patch:
                   patchSlotCheckGap gap gapStartSlot a
       else pure $ updateOnNewSlot lo a
     LogObject{loAt, loBody=LOTraceNodeIsLeader _} -> pure $
       a { aLeads = updateLeads loAt cur : rSLs
         }
     LogObject{loAt, loBody=LOResources rs} -> pure $
       -- Update resource stats accumulators & record values current slot.
       a { aResAccums = accs
         , aLeads = cur { slResources = extractResAccums accs
                        } : rSLs
         }
      where accs = updateResAccums loAt rs aResAccums
     LogObject{loBody=LOMempoolTxs txCount} -> pure $
       a { aMempoolTxs = txCount
         , aLeads      = cur { slMempoolTxs = txCount
                             } : rSLs
         }
     _ -> pure a
   go a = pure . const a

   updateOnNewSlot :: LogObject -> Analysis -> Analysis
   updateOnNewSlot LogObject{loAt, loBody=LOTraceStartLeadershipCheck slot utxo} a =
     extendAnalysis slot loAt 1 utxo a
   updateOnNewSlot _ _ =
     error "Internal invariant violated: updateSlot called for a non-LOTraceStartLeadershipCheck LogObject."

   patchSlotCheckGap :: Word64 -> Word64 -> Analysis -> Analysis
   patchSlotCheckGap 0 _ a = a
   patchSlotCheckGap n slot a@Analysis{aLeads=cur:_,..} =
     patchSlotCheckGap (n - 1) (slot + 1) $
     extendAnalysis slot (slotStart slot) 0 (slUtxoSize cur) a
   patchSlotCheckGap _ _ _ =
     error "Internal invariant violated: patchSlotCheckGap called with empty Analysis chain."

   extendAnalysis :: Word64 -> UTCTime -> Word64 -> Word64 -> Analysis -> Analysis
   extendAnalysis slot time checks utxo a@Analysis{..} =
     let (epoch, epochSlot) = slot `divMod` cpEpochSlots in
       a { aLeads = SlotStats
           { slSlot        = slot
           , slEpoch       = epoch
           , slEpochSlot   = epochSlot
           , slStart       = slotStart slot
           , slEarliest    = time
           , slOrderViol   = 0
             -- Updated as we see repeats:
           , slCountChecks = checks
           , slCountLeads  = 0
           , slSpan        = realToFrac (0 :: Int)
           , slMempoolTxs  = aMempoolTxs
           , slUtxoSize    = utxo
           , slResources   = extractResAccums aResAccums
           } : aLeads
         }

data Summary
  = Summary
    { sMaxChecks         :: !Word64
    , sSlotMisses        :: !(Seq Word64)
    , sMissDistrib       :: !(Distribution Float Float)
    , sLeadsDistrib      :: !(Distribution Float Word64)
    , sUtxoDistrib       :: !(Distribution Float Word64)
    , sCheckspanDistrib  :: !(Distribution Float NominalDiffTime)
    , sResourceDistribs  :: !(Resources (Distribution Float Word64))
    }
  deriving Show

instance ToJSON Summary where
  toJSON Summary{..} = Aeson.Array $ V.fromList
    [ extendObject "kind" "utxo"      $ toJSON sUtxoDistrib
    , extendObject "kind" "leads"     $ toJSON sLeadsDistrib
    , extendObject "kind" "checkspan" $ toJSON sCheckspanDistrib
    , extendObject "kind" "misses"    $ toJSON sMissDistrib
    , extendObject "kind" "cpu"       $ toJSON (rCentiCpu sResourceDistribs)
    , extendObject "kind" "gc"        $ toJSON (rCentiGC  sResourceDistribs)
    , extendObject "kind" "rss"       $ toJSON (rRSS      sResourceDistribs)
    ]

analysisSummary :: Seq SlotStats -> Summary
-- Insist on having at least three items: first, content and tail:
--   0th/last 5 slots are transient
analysisSummary (_ :<| (((((slots :|> _) :|> _) :|> _) :|> _) :|> _)) =
  Summary
  { sMaxChecks        = maxChecks
  , sSlotMisses       = misses
  , sMissDistrib      = computeDistribution pctiles missRatios
  , sLeadsDistrib     =
      computeDistribution pctiles (slCountLeads <$> slots)
  , sUtxoDistrib      =
      computeDistribution pctiles (slUtxoSize <$> slots)
  , sCheckspanDistrib =
      computeDistribution pctiles (slSpan <$> slots)
  , sResourceDistribs =
      computeResDistrib pctiles resDistProjs slots
  }
 where
   pctiles          = [0.5, 0.75, 0.95, 0.99]

   checkCounts      = slCountChecks <$> slots
   maxChecks        = maximum checkCounts
   misses           = (maxChecks -) <$> checkCounts
   missRatios       = missRatio <$> misses

   resDistProjs     =
     Resources
     { rCentiCpu    = rCentiCpu   . slResources
     , rCentiGC     = rCentiGC    . slResources
     , rCentiMut    = rCentiMut   . slResources
     , rGcsMajor    = rGcsMajor   . slResources
     , rGcsMinor    = rGcsMinor   . slResources
     , rAlloc       = rAlloc      . slResources
     , rLive        = rLive       . slResources
     , rRSS         = rRSS        . slResources
     , rCentiBlkIO  = rCentiBlkIO . slResources
     , rThreads     = rThreads    . slResources
     }

   missRatio :: Word64 -> Float
   missRatio = (/ fromIntegral maxChecks) . fromIntegral
analysisSummary _ = zeroSummary

data Analysis
  = Analysis
    { aResAccums  :: !ResAccums
    , aMempoolTxs :: !Word64
    , aLeads      :: ![SlotStats]
    }

data SlotStats
  = SlotStats
    { slSlot        :: !Word64
    , slEpoch       :: !Word64
    , slEpochSlot   :: !Word64
    , slStart       :: !UTCTime
    , slCountChecks :: !Word64
    , slCountLeads  :: !Word64
    , slOrderViol   :: !Word64
    , slEarliest    :: !UTCTime
    , slSpan        :: !NominalDiffTime
    , slMempoolTxs  :: !Word64
    , slUtxoSize    :: !Word64
    , slResources   :: !(Resources Word64)
    }
  deriving (Generic, Show)

instance ToJSON SlotStats

leadershipHeader :: Text
leadershipHeader =
  "abs.  slot     lead  leader check    %CPU      GCs   Produc-   Memory use, kB   Alloc rate Mempool  UTxO   Absolute" <>"\n"<>
  "slot#   epoch checks ships  span  all/GC/mut maj/min tivity  Live   Alloc   RSS  / mut sec  txs  entries   slot start time"

toLeadershipLine :: SlotStats -> Text
toLeadershipLine SlotStats{..} = Text.pack $
  printf "%5d %4d:%2d %4d    %2d %8s %3d %2d  %2d  %2d %2d  %0.2f %7d %7d %7d %7d %4d %9d  %s"
    slSlot
    slEpochSlot
    slEpoch
    slCountChecks
    slCountLeads
    (show slSpan :: Text)
    (rCentiCpu slResources)
    (rCentiGC  slResources)
    (rCentiMut slResources)
    (rGcsMajor slResources)
    (rGcsMinor slResources)
    (fromIntegral (rCentiMut slResources) / fromIntegral (rCentiCpu slResources) :: Float)
    (rLive     slResources `div` 1024)
    (rAlloc    slResources `div` 1024)
    (rRSS      slResources * 4)
    ((ceiling :: Float -> Int) $ fromIntegral (100 * rAlloc slResources) / fromIntegral (rCentiMut slResources * 1024))
    slMempoolTxs
    slUtxoSize
    (show slStart :: Text)

zeroAnalysis :: Analysis
zeroAnalysis =
  Analysis
  { aResAccums  = mkResAccums
  , aMempoolTxs = 0
  , aLeads      = [zeroLeadership]
  }
 where
   zeroLeadership =
     SlotStats
     { slSlot = 0
     , slEpoch = 0
     , slEpochSlot = 0
     , slStart = zeroUTCTime
     , slCountChecks = 0
     , slCountLeads = 0
     , slOrderViol = 0
     , slEarliest = zeroUTCTime
     , slSpan = realToFrac (0 :: Int)
     , slMempoolTxs = 0
     , slUtxoSize = 0
     , slResources = pure 0
     }

zeroSummary :: Summary
zeroSummary =
  Summary
  { sMaxChecks        = 0
  , sSlotMisses       = mempty
  , sMissDistrib      = zeroDistribution
  , sLeadsDistrib     = zeroDistribution
  , sUtxoDistrib      = zeroDistribution
  , sCheckspanDistrib = zeroDistribution
  , sResourceDistribs = pure zeroDistribution
  }
