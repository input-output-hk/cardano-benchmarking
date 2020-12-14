{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Unlog.Analysis (module Cardano.Unlog.Analysis) where

import           Prelude (error)
import           Cardano.Prelude

import qualified Data.Sequence as Seq

import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time

import           Data.Accum
import           Cardano.Unlog.ChainParams
import           Cardano.Unlog.LogObject
import           Cardano.Unlog.Resources
import           Cardano.Unlog.SlotStats


data Analysis
  = Analysis
    { aResAccums     :: !ResAccums
    , aResTimestamp  :: !UTCTime
    , aMempoolTxs    :: !Word64
    , aBlockNo       :: !Word64
    , aLastBlockSlot :: !Word64
    , aSlotStats     :: ![SlotStats]
    }

analyseLogObjects :: ChainParams -> [LogObject] -> Seq SlotStats
analyseLogObjects cp =
  Seq.reverse . Seq.fromList . aSlotStats
  . foldl (analysisStep cp) zeroAnalysis
 where
   zeroAnalysis :: Analysis
   zeroAnalysis =
     Analysis
     { aResAccums     = mkResAccums
     , aResTimestamp  = zeroUTCTime
     , aMempoolTxs    = 0
     , aBlockNo       = 0
     , aLastBlockSlot = 0
     , aSlotStats     = [zeroSlotStats]
     }

analysisStep :: ChainParams -> Analysis -> LogObject -> Analysis
analysisStep cp a@Analysis{aSlotStats=cur:rSLs, ..} = \case
  lo@LogObject{loAt, loBody=LOTraceStartLeadershipCheck slot _ _} ->
    if slSlot cur > slot
    -- Slot log entry for a slot we've supposedly done processing.
    then a { aSlotStats = cur
             { slOrderViol = slOrderViol cur + 1
             } : case (slSlot cur - slot, rSLs) of
                   -- Limited back-patching:
                   (1, p1:rest)       ->       updateChecks loAt p1:rest
                   (2, p1:p2:rest)    ->    p1:updateChecks loAt p2:rest
                   (3, p1:p2:p3:rest) -> p1:p2:updateChecks loAt p3:rest
                   _ -> rSLs -- Give up.
           }
    else if slSlot cur == slot
    then a { aSlotStats = updateChecks loAt cur : rSLs
           }
    else if slot - slSlot cur > 1
    then let gap = slot - slSlot cur - 1
             gapStartSlot = slSlot cur + 1 in
         updateOnNewSlot lo $ -- We have a slot check gap to patch:
         patchSlotCheckGap gap gapStartSlot a
    else updateOnNewSlot lo a
  LogObject{loAt, loBody=LOTraceNodeIsLeader _} ->
    a { aSlotStats = updateSlotStats loAt cur : rSLs
      }
  LogObject{loAt, loBody=LOResources rs} ->
    -- Update resource stats accumulators & record values current slot.
    a { aResAccums = accs
      , aResTimestamp = loAt
      , aSlotStats = cur { slResources = Just <$> extractResAccums accs
                     } : rSLs
      }
   where accs = updateResAccums loAt rs aResAccums
  LogObject{loBody=LOMempoolTxs txCount} ->
    a { aMempoolTxs     = txCount
      , aSlotStats      = cur { slMempoolTxs = txCount
                          } : rSLs
      }
  LogObject{loBody=LOBlockContext blockNo} ->
    let newBlock = aBlockNo /= blockNo in
    a { aBlockNo        = blockNo
      , aLastBlockSlot  = if newBlock
                          then slSlot cur
                          else aLastBlockSlot
      , aSlotStats      = cur { slBlockNo = blockNo
                              , slBlockless = if newBlock
                                              then 0
                                              else slBlockless cur
                              } : rSLs
      }
  LogObject{loBody=LOLedgerTookSnapshot} ->
    a { aSlotStats      = cur { slChainDBSnap = slChainDBSnap cur + 1
                              } : rSLs
      }
  LogObject{loBody=LOMempoolRejectedTx} ->
    a { aSlotStats      = cur { slRejectedTx = slRejectedTx cur + 1
                              } : rSLs
      }
  _ -> a
 where
   updateOnNewSlot :: LogObject -> Analysis -> Analysis
   updateOnNewSlot LogObject{loAt, loBody=LOTraceStartLeadershipCheck slot utxo density} a' =
     extendAnalysis cp slot loAt 1 utxo density a'
   updateOnNewSlot _ _ =
     error "Internal invariant violated: updateSlot called for a non-LOTraceStartLeadershipCheck LogObject."

   updateChecks :: UTCTime -> SlotStats -> SlotStats
   updateChecks now sl@SlotStats{..} =
     sl { slCountChecks = slCountChecks + 1
        , slSpan  = now `Time.diffUTCTime` slStart
        }

   updateSlotStats :: UTCTime -> SlotStats -> SlotStats
   updateSlotStats now sl@SlotStats{..} =
     sl { slCountLeads = slCountLeads + 1
        , slSpan  = now `Time.diffUTCTime` slStart
        }

   patchSlotCheckGap :: Word64 -> Word64 -> Analysis -> Analysis
   patchSlotCheckGap 0 _ a' = a'
   patchSlotCheckGap n slot a'@Analysis{aSlotStats=cur':_} =
     patchSlotCheckGap (n - 1) (slot + 1) $
     extendAnalysis cp slot (slotStart cp slot) 0 (slUtxoSize cur') (slDensity cur') a'
   patchSlotCheckGap _ _ _ =
     error "Internal invariant violated: patchSlotCheckGap called with empty Analysis chain."
analysisStep _ a = const a

extendAnalysis ::
     ChainParams
  -> Word64 -> UTCTime -> Word64 -> Word64 -> Float
  -> Analysis -> Analysis
extendAnalysis cp@ChainParams{..} slot time checks utxo density a@Analysis{..} =
  let (epoch, epochSlot) = slot `divMod` cpEpochSlots in
    a { aSlotStats = SlotStats
        { slSlot        = slot
        , slEpoch       = epoch
        , slEpochSlot   = epochSlot
        , slStart       = slotStart cp slot
        , slEarliest    = time
        , slOrderViol   = 0
          -- Updated as we see repeats:
        , slCountChecks = checks
        , slCountLeads  = 0
        , slSpan        = time `Time.diffUTCTime` slotStart cp slot
        , slMempoolTxs  = aMempoolTxs
        , slUtxoSize    = utxo
        , slDensity     = density
        , slChainDBSnap = 0
        , slRejectedTx  = 0
        , slBlockNo     = aBlockNo
        , slBlockless   = slot - aLastBlockSlot
        , slResources   = maybeDiscard
                          <$> discardObsoleteValues
                          <*> extractResAccums aResAccums
        } : aSlotStats
      }
    where maybeDiscard :: (Word64 -> Maybe Word64) -> Word64 -> Maybe Word64
          maybeDiscard f = f

slotStart :: ChainParams -> Word64 -> UTCTime
slotStart ChainParams{..} = flip Time.addUTCTime cpSystemStart . (* cpSlotLength) . fromIntegral
