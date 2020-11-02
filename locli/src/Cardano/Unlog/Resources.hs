{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Unlog.Resources
  ( ResAccums
  , mkResAccums
  , updateResAccums
  , extractResAccums
  , ResDistribProjections
  , computeResDistrib
  , ResContinuity
  , discardObsoleteValues
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
  , rAlloc       = mkAccumDelta `divAccum` 1024
  , rLive        = mkAccumNew   `divAccum` 1024
  , rRSS         = mkAccumNew   `divAccum` 1024
  , rCentiBlkIO  = mkAccumTicksShare
  , rThreads     = mkAccumNew
  }

updateResAccums :: UTCTime -> ResourceStats -> ResAccums -> ResAccums
updateResAccums now rs ra =
  updateAccum now <$> rs <*> ra

-- | Obtain the current values in resource accumulators.
extractResAccums :: ResAccums -> Resources Word64
extractResAccums = (aCurrent <$>)

type ResDistribProjections a = Resources (a -> Maybe Word64)

computeResDistrib ::
  forall a
  .  [PercSpec Float]
  -> ResDistribProjections a
  -> Seq.Seq a
  -> Resources (Distribution Float Word64)
computeResDistrib percentiles projs xs =
  compDist <$> projs
 where
   compDist :: (a -> Maybe Word64) -> Distribution Float Word64
   compDist proj = computeDistribution percentiles
     (Seq.fromList . catMaybes . toList $ proj <$> xs)

type ResContinuity a = Resources (a -> Maybe a)

discardObsoleteValues :: ResContinuity a
discardObsoleteValues =
  Resources
  { rCentiCpu    = const Nothing
  , rCentiGC     = const Nothing
  , rCentiMut    = const Nothing
  , rGcsMajor    = const Nothing
  , rGcsMinor    = const Nothing
  , rAlloc       = const Nothing
  , rLive        = Just
  , rRSS         = Just
  , rCentiBlkIO  = const Nothing
  , rThreads     = Just
  }
