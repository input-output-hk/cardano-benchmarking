{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Unlog.Summary
  ( AnalysisCmdError
  , renderAnalysisCmdError
  , runAnalysisCommand
  ) where

import           Prelude (String)
import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT)

import qualified Data.Aeson as Aeson
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Vector as V

import qualified Graphics.Histogram as Hist
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import           Data.Time.Clock (NominalDiffTime)
import           Text.Printf

import           Data.Distribution
import           Cardano.Unlog.Analysis
import           Cardano.Unlog.Commands
import           Cardano.Unlog.LogObject
import           Cardano.Unlog.Resources
import           Cardano.Unlog.SlotStats


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
runAnalysisCommand (LeadershipChecks startTime mJDumpFile mPDumpFile mETimelineFile mEStatsFile mHistoOutFile mJOutFile logfiles) =
  firstExceptT AnalysisCmdError $
    runLeadershipCheckCmd startTime mJDumpFile mPDumpFile mETimelineFile mEStatsFile mHistoOutFile mJOutFile logfiles
runAnalysisCommand SubstringKeys =
  liftIO $ mapM_ putStrLn logObjectStreamInterpreterKeys

runLeadershipCheckCmd ::
     ChainParams
  -> Maybe JsonOutputFile
  -> Maybe TextOutputFile
  -> Maybe TextOutputFile
  -> Maybe TextOutputFile
  -> Maybe EpsOutputFile
  -> Maybe JsonOutputFile
  -> [JsonLogfile]
  -> ExceptT Text IO ()
runLeadershipCheckCmd cp mLeadershipsDumpFile mPrettyDumpFile mExportTimelineFile mExportStatsFile mHistoOutFile mJOutFile logfiles = do
  objs :: [LogObject] <- liftIO $
    concat <$> mapM readLogObjectStream logfiles
  -- liftIO $ withFile "lodump.json" WriteMode $ \hnd ->
  --   forM_ objs $ LBS.hPutStrLn hnd . Aeson.encode
  let noisySlotStats = analyseLogObjects cp objs
  liftIO $ do
    case mLeadershipsDumpFile of
      Nothing -> pure ()
      Just (JsonOutputFile f) ->
        withFile f WriteMode $ \hnd ->
          forM_ noisySlotStats $ LBS.hPutStrLn hnd . Aeson.encode
    let slotStats = cleanupSlotStats noisySlotStats
        summary :: Summary
        summary = slotStatsSummary slotStats
        analysisOutput :: LBS.ByteString
        analysisOutput = Aeson.encode summary
    maybe (pure ()) (renderPrettySummary slotStats summary logfiles) mPrettyDumpFile
    maybe (pure ()) (renderExportStats summary) mExportStatsFile
    maybe (pure ()) (renderExportTimeline slotStats) mExportTimelineFile
    maybe (pure ())
      (renderHistogram "CPU usage spans over 85%" "Span length"
        (toList $ Seq.sort $ sSpanLensCPU85 summary))
      mHistoOutFile
    case mJOutFile of
      Nothing -> LBS.putStrLn analysisOutput
      Just (JsonOutputFile f) ->
        withFile f WriteMode $ \hnd ->
          LBS.hPutStrLn hnd analysisOutput
 where
   renderHistogram :: Integral a
     => String -> String -> [a] -> EpsOutputFile -> IO ()
   renderHistogram desc ylab xs (EpsOutputFile f) =
     Hist.plotAdv f opts hist >> pure ()
    where
      hist = Hist.histogram Hist.binFreedmanDiaconis $ fromIntegral <$> xs
      opts = Opts.title desc $ Opts.yLabel ylab $ Opts.xLabel "Population" $
             Hist.defOpts hist

   renderPrettySummary ::
        Seq SlotStats -> Summary -> [JsonLogfile] -> TextOutputFile -> IO ()
   renderPrettySummary xs s srcs o =
     withFile (unTextOutputFile o) WriteMode $ \hnd -> do
       hPutStrLn hnd . Text.pack $
         printf "--- input: %s" (intercalate " " $ unJsonLogfile <$> srcs)
       renderStats   statsHeadP statsFormatP s hnd
       renderSlotTimeline slotHeadP slotFormatP False xs hnd
   renderExportStats :: Summary -> TextOutputFile -> IO ()
   renderExportStats s o =
     withFile (unTextOutputFile o) WriteMode $
       renderStats statsHeadE statsFormatE s
   renderExportTimeline :: Seq SlotStats -> TextOutputFile -> IO ()
   renderExportTimeline xs o =
     withFile (unTextOutputFile o) WriteMode $
       renderSlotTimeline slotHeadE slotFormatE True xs

   renderStats :: Text -> Text -> Summary -> Handle -> IO ()
   renderStats statHead statFmt summary hnd = do
       hPutStrLn hnd statHead
       forM_ (toDistribLines statFmt summary) $
         hPutStrLn hnd

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
    , sBlocklessDistrib  :: !(Distribution Float Word64)
    , sSpanLensCPU85Distrib
                         :: !(Distribution Float Int)
    , sResourceDistribs  :: !(Resources (Distribution Float Word64))
    }
  deriving Show

instance ToJSON Summary where
  toJSON Summary{..} = Aeson.Array $ V.fromList
    [ Aeson.Object $ HashMap.fromList
        [ "kind" .= String "spanLensCPU85"
        , "xs" .= toJSON sSpanLensCPU85]
    , Aeson.Object $ HashMap.fromList
        [ "kind" .= String "spanLensCPU85Sorted"
        , "xs" .= toJSON (Seq.sort sSpanLensCPU85)]
    , extendObject "kind" "checkspan" $ toJSON sCheckspanDistrib
    , extendObject "kind" "cpu"       $ toJSON (rCentiCpu sResourceDistribs)
    , extendObject "kind" "gc"        $ toJSON (rCentiGC  sResourceDistribs)
    , extendObject "kind" "density"   $ toJSON sDensityDistrib
    , extendObject "kind" "utxo"      $ toJSON sUtxoDistrib
    , extendObject "kind" "leads"     $ toJSON sLeadsDistrib
    , extendObject "kind" "misses"    $ toJSON sMissDistrib
    , extendObject "kind" "blockless" $ toJSON sBlocklessDistrib
    , extendObject "kind" "rss"       $ toJSON (rRSS      sResourceDistribs)
    , extendObject "kind" "spanLensCPU85Distrib"  $
                                        toJSON sSpanLensCPU85Distrib
    ]

slotStatsSummary :: Seq SlotStats -> Summary
slotStatsSummary slots =
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
  , sBlocklessDistrib =
      computeDistribution pctiles (slBlockless <$> slots)
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
   maxChecks        = if length checkCounts == 0
                      then 0 else maximum checkCounts
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
    <*> ZipList (pctSample <$> dPercentiles sBlocklessDistrib)
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
     -> Float -> NominalDiffTime -> Word64 -> Float
     -> Word64 -> Word64 -> Word64
     -> Word64 -> Word64
     -> Word64 -> Word64 -> Word64
     -> Int -> Int -> Int
     -> Text
   distribLine ps count miss chkdt' blkl dens cpu gc mut majg ming liv alc rss cpu85Sp cpu85SpIdx cpu85SpPrev = Text.pack $
     printf (Text.unpack statsF)
    (renderPercSpec 6 ps) count miss chkdt blkl dens cpu gc mut majg ming     liv alc rss cpu85Sp cpu85SpIdx cpu85SpPrev
    where chkdt = show chkdt' :: Text

statsHeadE, statsFormatE :: Text
statsHeadP, statsFormatP :: Text
statsHeadP =
  "%tile Count MissR  CheckΔt   BlkLess Dens  CPU  GC MUT Maj Min         Live   Alloc   RSS    CPU85%-SpanLengths/Idx/Prev"
statsHeadE =
  "%tile,Count,MissR,CheckΔ,Blockless,ChainDensity,CPU,GC,MUT,GcMaj,GcMin,Live,Alloc,RSS,CPU85%-SpanLens,/Idx,/Prev"
statsFormatP =
  "%6s %5d %0.2f   %6s  %3d     %0.3f  %3d %3d %3d %2d %3d      %8d %8d %7d %4d %4d %4d"
statsFormatE =
  "%s,%d,%0.2f,%s,%d,%0.3f,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d"
