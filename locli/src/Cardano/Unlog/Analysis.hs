{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
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

import           Control.Applicative (ZipList(..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import qualified Data.Aeson as Aeson
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HashMap
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
runAnalysisCommand (LeadershipChecks startTime mJDumpFile mPDumpFile mEDumpFile mJOutFile logfiles) =
  firstExceptT AnalysisCmdError $
    runLeadershipCheckCmd startTime mJDumpFile mPDumpFile mEDumpFile mJOutFile logfiles
runAnalysisCommand SubstringKeys =
  liftIO $ mapM_ putStrLn logObjectStreamInterpreterKeys

runLeadershipCheckCmd ::
     ChainParams
  -> Maybe JsonOutputFile
  -> Maybe TextOutputFile
  -> Maybe TextOutputFile
  -> Maybe JsonOutputFile
  -> [JsonLogfile]
  -> ExceptT Text IO ()
runLeadershipCheckCmd ChainParams{..}
 mLeadershipsDumpFile mPrettyDumpFile mExportDumpFile mJOutFile logfiles = do
  objs :: [LogObject] <- liftIO $
    concat <$> mapM readLogObjectStream logfiles
  -- liftIO $ withFile "lodump.json" WriteMode $ \hnd ->
  --   forM_ objs $ LBS.hPutStrLn hnd . Aeson.encode
  noisyLeads <- computeLeadershipStats objs
  liftIO $ do
    case mLeadershipsDumpFile of
      Nothing -> pure ()
      Just (JsonOutputFile f) ->
        withFile f WriteMode $ \hnd ->
          forM_ noisyLeads $ LBS.hPutStrLn hnd . Aeson.encode
    let leads = cleanupSlotStats noisyLeads
        summary :: Summary
        summary = analysisSummary leads
        analysisOutput :: LBS.ByteString
        analysisOutput = Aeson.encode summary
    maybe (pure ()) (renderSummaryPretty False leads summary logfiles) mPrettyDumpFile
    maybe (pure ()) (renderSummaryExport  True leads summary logfiles) mExportDumpFile
    case mJOutFile of
      Nothing -> LBS.putStrLn analysisOutput
      Just (JsonOutputFile f) ->
        withFile f WriteMode $ \hnd ->
          LBS.hPutStrLn hnd analysisOutput
 where
   renderSummaryPretty, renderSummaryExport ::
        Bool -> Seq SlotStats -> Summary -> [JsonLogfile] -> TextOutputFile -> IO ()
   renderSummaryPretty =
     renderSummary statsHeadP statsFormatP leadershipHeadP leadershipFormatP
   renderSummaryExport =
     renderSummary statsHeadE statsFormatE leadershipHeadE leadershipFormatE

   renderSummary ::
        Text -> Text -> Text -> Text -> Bool
     -> Seq SlotStats -> Summary -> [JsonLogfile] -> TextOutputFile -> IO ()
   renderSummary statHead statFmt leadHead leadFmt exportMode leads summary srcs outf = do
     withFile (unTextOutputFile outf) WriteMode $ \hnd -> do
       unless exportMode $
         hPutStrLn hnd . Text.pack $
           printf "--- input: %s" (intercalate " " $ unJsonLogfile <$> srcs)
       hPutStrLn hnd statHead
       forM_ (toDistribLines statFmt summary) $
         hPutStrLn hnd
       forM_ (zip (toList leads) [(0 :: Int)..]) $ \(l, i) -> do
         when (i `mod` 33 == 0) $
           hPutStrLn hnd leadHead
         hPutStrLn hnd $ toLeadershipLine exportMode leadFmt l

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
     lo@LogObject{loAt, loBody=LOTraceStartLeadershipCheck slot _ _} ->
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
         , aResTimestamp = loAt
         , aLeads = cur { slResources = Just <$> extractResAccums accs
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
   updateOnNewSlot LogObject{loAt, loBody=LOTraceStartLeadershipCheck slot utxo density} a =
     extendAnalysis slot loAt 1 utxo density a
   updateOnNewSlot _ _ =
     error "Internal invariant violated: updateSlot called for a non-LOTraceStartLeadershipCheck LogObject."

   patchSlotCheckGap :: Word64 -> Word64 -> Analysis -> Analysis
   patchSlotCheckGap 0 _ a = a
   patchSlotCheckGap n slot a@Analysis{aLeads=cur:_,..} =
     patchSlotCheckGap (n - 1) (slot + 1) $
     extendAnalysis slot (slotStart slot) 0 (slUtxoSize cur) (slDensity cur) a
   patchSlotCheckGap _ _ _ =
     error "Internal invariant violated: patchSlotCheckGap called with empty Analysis chain."

   extendAnalysis ::
        Word64 -> UTCTime -> Word64 -> Word64 -> Float
     -> Analysis -> Analysis
   extendAnalysis slot time checks utxo density a@Analysis{..} =
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
           , slSpan        = time `Time.diffUTCTime` slotStart slot
           , slMempoolTxs  = aMempoolTxs
           , slUtxoSize    = utxo
           , slDensity     = density
           , slResources   = maybeDiscard
                             <$> discardObsoleteValues
                             <*> extractResAccums aResAccums
           } : aLeads
         }
       where maybeDiscard :: (Word64 -> Maybe Word64) -> Word64 -> Maybe Word64
             maybeDiscard f = f

data Summary
  = Summary
    { sMaxChecks         :: !Word64
    , sSlotMisses        :: !(Seq Word64)
    , sSpanLensCPU85     :: !(Seq Int)
    -- distributions
    , sMissDistrib       :: !(Distribution Float Float)
    , sLeadsDistrib      :: !(Distribution Float Word64)
    , sUtxoDistrib       :: !(Distribution Float Word64)
    , sDensityDistrib    :: !(Distribution Float Float)
    , sCheckspanDistrib  :: !(Distribution Float NominalDiffTime)
    , sSpanLensCPU85Distrib
                         :: !(Distribution Float Int)
    , sResourceDistribs  :: !(Resources (Distribution Float Word64))
    }
  deriving Show

instance ToJSON Summary where
  toJSON Summary{..} = Aeson.Array $ V.fromList
    [ extendObject "kind" "utxo"      $ toJSON sUtxoDistrib
    , extendObject "kind" "density"   $ toJSON sDensityDistrib
    , extendObject "kind" "leads"     $ toJSON sLeadsDistrib
    , extendObject "kind" "checkspan" $ toJSON sCheckspanDistrib
    , extendObject "kind" "misses"    $ toJSON sMissDistrib
    , extendObject "kind" "cpu"       $ toJSON (rCentiCpu sResourceDistribs)
    , extendObject "kind" "gc"        $ toJSON (rCentiGC  sResourceDistribs)
    , extendObject "kind" "rss"       $ toJSON (rRSS      sResourceDistribs)
    , extendObject "kind" "spanLensCPU85Distrib"  $ toJSON sSpanLensCPU85Distrib
    , Aeson.Object $ HashMap.fromList
        [ "kind" .= String "spanLensCPU85", "xs" .= toJSON sSpanLensCPU85]
    ]

-- | Initial and trailing data are noisy outliers: drop that.
--
--   The initial part is useless until the node actually starts
--   to interact with the blockchain, so we drop all slots until
--   they start getting non-zero chain density reported.
--
--   On the trailing part, we drop everything since the last leadership check.
cleanupSlotStats :: Seq SlotStats -> Seq SlotStats
cleanupSlotStats =
  Seq.dropWhileL ((== 0) . slDensity) .
  Seq.dropWhileR ((== 0) . slCountChecks)

analysisSummary :: Seq SlotStats -> Summary
analysisSummary slots =
  Summary
  { sMaxChecks        = maxChecks
  , sSlotMisses       = misses
  , sSpanLensCPU85    = spanLensCPU85
  --
  , sMissDistrib      = computeDistribution pctiles missRatios
  , sLeadsDistrib     =
      computeDistribution pctiles (slCountLeads <$> slots)
  , sUtxoDistrib      =
      computeDistribution pctiles (slUtxoSize <$> slots)
  , sDensityDistrib   =
      computeDistribution pctiles (slDensity <$> slots)
  , sCheckspanDistrib =
      computeDistribution pctiles (slSpan <$> slots)
  , sSpanLensCPU85Distrib
                      = computeDistribution pctiles spanLensCPU85
  , sResourceDistribs =
      computeResDistrib pctiles resDistProjs slots
  }
 where
   pctiles = sortBy (compare `on` psFrac)
     [ PercAnon 0.01, PercAnon 0.05
     , PercAnon 0.1, PercAnon 0.2, PercAnon 0.3, PercAnon 0.4
     , PercAnon 0.5, PercAnon 0.6, PercAnon 0.7, PercAnon 0.8, PercAnon 0.9
     , PercAnon 0.95, PercAnon 0.97, PercAnon 0.98, PercAnon 0.99
     , PercAnon 0.995, PercAnon 0.997, PercAnon 0.998, PercAnon 0.999
     , PercAnon 0.9995, PercAnon 0.9997, PercAnon 0.9998, PercAnon 0.9999
     ]

   checkCounts      = slCountChecks <$> slots
   maxChecks        = maximum checkCounts
   misses           = (maxChecks -) <$> checkCounts
   missRatios       = missRatio <$> misses
   spanLensCPU85    = Seq.fromList $ length <$>
                        spans (>=85)
                          ((rCentiCpu . slResources <$> toList slots)
                           & catMaybes)

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

data Analysis
  = Analysis
    { aResAccums    :: !ResAccums
    , aResTimestamp :: !UTCTime
    , aMempoolTxs   :: !Word64
    , aLeads        :: ![SlotStats]
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
    , slDensity     :: !Float
    , slResources   :: !(Resources (Maybe Word64))
    }
  deriving (Generic, Show)

instance ToJSON SlotStats

toDistribLines :: Text -> Summary -> [Text]
toDistribLines statsF Summary{..} =
  getZipList $
  distribLine
    <$> ZipList (pctSpec <$> dPercentiles sMissDistrib)
    <*> ZipList (max 1 . ceiling . (* fromIntegral (dCount sMissDistrib))
                 . (1.0 -) . pctFrac
                     <$> dPercentiles sMissDistrib)
    <*> ZipList (pctSample <$> dPercentiles sMissDistrib)
    <*> ZipList (pctSample <$> dPercentiles sCheckspanDistrib)
    <*> ZipList (pctSample <$> dPercentiles sDensityDistrib)
    <*> ZipList (pctSample <$> dPercentiles (rCentiCpu sResourceDistribs))
    <*> ZipList (pctSample <$> dPercentiles (rCentiGC sResourceDistribs))
    <*> ZipList (pctSample <$> dPercentiles (rCentiMut sResourceDistribs))
    <*> ZipList (pctSample <$> dPercentiles (rGcsMajor sResourceDistribs))
    <*> ZipList (pctSample <$> dPercentiles (rGcsMinor sResourceDistribs))
    -- <*> ZipList (pctSample <$> dPercentiles ( sResourceDistribs))
    <*> ZipList (pctSample <$> dPercentiles (rLive sResourceDistribs))
    <*> ZipList (pctSample <$> dPercentiles (rAlloc sResourceDistribs))
    <*> ZipList (pctSample <$> dPercentiles (rRSS sResourceDistribs))
    <*> ZipList (pctSample <$> dPercentiles sSpanLensCPU85Distrib)
    <*> ZipList (pctSampleIndex
                           <$> dPercentiles sSpanLensCPU85Distrib)
    <*> ZipList (pctSamplePrev
                           <$> dPercentiles sSpanLensCPU85Distrib)
 where
   distribLine ::
        PercSpec Float -> Int
     -> Float -> NominalDiffTime -> Float
     -> Word64 -> Word64 -> Word64
     -> Word64 -> Word64
     -> Word64 -> Word64 -> Word64
     -> Int -> Int -> Int
     -> Text
   distribLine ps count miss chkdt' dens cpu gc mut majg ming liv alc rss cpu85Sp cpu85SpIdx cpu85SpPrev = Text.pack $
     printf (Text.unpack statsF)
    (renderPercSpec 6 ps) count miss chkdt dens cpu gc mut majg ming     liv alc rss cpu85Sp cpu85SpIdx cpu85SpPrev
    where chkdt = show chkdt' :: Text

statsHeadE, statsFormatE, leadershipHeadE, leadershipFormatE :: Text
statsHeadP, statsFormatP, leadershipHeadP, leadershipFormatP :: Text
statsHeadP =
  "%tile Count MissR  CheckΔt   Dens  CPU  GC MUT Maj Min         Live   Alloc   RSS    CPU85%-SpanLengths/Idx/Prev"
statsHeadE =
  "%tile Count MissR CheckΔ ChainDensity CPU GC MUT GcMaj GcMin Live Alloc RSS CPU85%-SpanLengths/Idx/Prev"
statsFormatP =
  "%6s %5d %0.2f   %6s  %0.3f  %3d %3d %3d %2d %3d      %8d %8d %7d %4d %4d %4d"
statsFormatE =
  "%s %d %0.2f %s %0.3f %d %d %d %d %d %d %d %d %d %d %d"
leadershipHeadP =
  "abs.  slot     lead  leader check chain       %CPU      GCs   Produc-   Memory use, kB      Alloc rate  Mempool  UTxO   Absolute" <>"\n"<>
  "slot#   epoch checks ships  span  density all/ GC/mut maj/min tivity  Live   Alloc   RSS     / mut sec   txs  entries   slot start time"
leadershipHeadE =
  "abs.slot# slot epoch leadChecks leadShips chainDens %CPU %GC %MUT Productiv MemLiveKb MemAllocKb MemRSSKb AllocRate/Mut MempoolTxs UTxO AbsoluteSlotTime"
leadershipFormatP = "%5d %4d:%2d %4d    %2d %8s  %0.3f  %3s %3s %3s %2s %3s   %4s %7s %7s %7s % 8s %4d %9d  %s"
leadershipFormatE = "%d %d %d %d %d %s %0.3f %s %s %s %s %s %s %s %s %s %s %d %d %s"

toLeadershipLine :: Bool -> Text -> SlotStats -> Text
toLeadershipLine exportMode leadershipF SlotStats{..} = Text.pack $
  printf (Text.unpack leadershipF)
         sl epsl epo chks  lds span dens cpu gc mut majg ming   pro liv alc rss atm mpo utx star
 where sl   = slSlot
       epsl = slEpochSlot
       epo  = slEpoch
       chks = slCountChecks
       lds  = slCountLeads
       span = show slSpan :: Text
       cpu  = d 3 $ rCentiCpu slResources
       dens = slDensity
       gc   = d 2 $ rCentiGC  slResources
       mut  = d 2 $ rCentiMut slResources
       majg = d 2 $ rGcsMajor slResources
       ming = d 2 $ rGcsMinor slResources
       pro  = f 2 $ calcProd <$> (fromIntegral <$> rCentiMut slResources :: Maybe Float)
                             <*> (fromIntegral <$> rCentiCpu slResources)
       liv  = d 7 (rLive     slResources)
       alc  = d 7 (rAlloc    slResources)
       rss  = d 7 (rRSS      slResources)
       atm  = d 8 $
              (ceiling :: Float -> Int)
              <$> ((/) <$> (fromIntegral . (100 *) <$> rAlloc slResources)
                       <*> (fromIntegral . max 1 . (1024 *) <$> rCentiMut slResources))
       mpo  = slMempoolTxs
       utx  = slUtxoSize
       star = show slStart :: Text

       calcProd :: Float -> Float -> Float
       calcProd mut' cpu' = if cpu' == 0 then 1 else mut' / cpu'

       d, f :: PrintfArg a => Int -> Maybe a -> Text
       d width = \case
         Just x  -> Text.pack $ printf ("%"<>(if exportMode then "0" else "")
                                           <>show width<>"d") x
         Nothing -> mconcat (replicate width "-")
       f width = \case
         Just x  -> Text.pack $ printf ("%0."<>show width<>"f") x
         Nothing -> mconcat (replicate width "-")

zeroAnalysis :: Analysis
zeroAnalysis =
  Analysis
  { aResAccums    = mkResAccums
  , aResTimestamp = zeroUTCTime
  , aMempoolTxs   = 0
  , aLeads        = [zeroLeadership]
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
     , slDensity = 0
     , slResources = pure Nothing
     }
