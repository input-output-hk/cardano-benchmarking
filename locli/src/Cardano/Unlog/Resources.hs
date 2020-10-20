{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Unlog.Resources
  ( ResAccums
  , mkResAccums
  , updateResAccums
  , extractResAccums
  , ResDistribProjections
  , computeResDistrib
  -- * Re-exports
  , Resources(..)
  ) where

import           Cardano.Prelude

import           Data.Accum
import           Data.Distribution
import qualified Data.Sequence as Seq
import           Data.Time.Clock (UTCTime)

import           Cardano.BM.Stats.Resources

-- | Resouce accumulators
type ResAccums = Resources (Accum Word64 Word64)

mkResAccums :: ResAccums
mkResAccums =
  Resources
  { rCentiCpu    = mkAccumTicksShare
  , rCentiGC     = mkAccumTicksShare
  , rCentiMut    = mkAccumTicksShare
  , rGcsMajor    = mkAccumDelta
  , rGcsMinor    = mkAccumDelta
  , rAlloc       = mkAccumDelta
  , rLive        = mkAccumNew
  , rRSS         = mkAccumNew
  , rCentiBlkIO  = mkAccumTicksShare
  , rThreads     = mkAccumNew
  }

updateResAccums :: UTCTime -> ResourceStats -> ResAccums -> ResAccums
updateResAccums now rs ra =
  updateAccum now <$> rs <*> ra

-- | Obtain the current values in resource accumulators.
extractResAccums :: ResAccums -> Resources Word64
extractResAccums = (aCurrent <$>)

type ResDistribProjections a = Resources (a -> Word64)

computeResDistrib ::
  forall a
  .  [Float]
  -> ResDistribProjections a
  -> Seq.Seq a
  -> Resources (Distribution Float Word64)
computeResDistrib percentiles projs xs =
  compDist <$> projs
 where
   compDist :: (a -> Word64) -> Distribution Float Word64
   compDist proj = computeDistribution percentiles (proj <$> xs)
