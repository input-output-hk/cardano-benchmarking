{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Accum
  ( Accum(..)
  , mkAccum
  , updateAccum
  , zeroUTCTime
  -- Various accumulators
  , mkAccumNew
  , mkAccumDelta
  , mkAccumTicksShare
  ) where

import           Cardano.Prelude

import           Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)
import qualified Data.Time.Clock.POSIX as Time

data Accum a b
  = Accum
    { aUpdate    :: !(NominalDiffTime -> a -> a -> b)
    , aPrevStamp :: !UTCTime
    , aPrevValue :: !a
    , aCurrent   :: !b
    }

mkAccum :: a -> b -> (NominalDiffTime -> a -> a -> b) -> Accum a b
mkAccum a b update = Accum update zeroUTCTime a b

updateAccum :: UTCTime -> a -> Accum a b -> Accum a b
updateAccum now val a@Accum{..} =
  a
  { aPrevStamp = now
  , aPrevValue = val
  , aCurrent   = aUpdate elapsed aPrevValue val
  }
  where elapsed = now `diffUTCTime` aPrevStamp

-- updateCounter :: UTCTime -> Word -> CpuTickCounter -> CpuTickCounter
-- updateCounter now newValue CpuTickCounter{..} =
--   Counter
--   { cPrevStamp   = now
--   , cPrevValue   = newValue
--   , cCurShare    = newFraction
--   }
--   where newFraction :: Float
--         newFraction    = fromIntegral spentTicks / elapsedTicks
--         spentTicks     = newValue - ctcPrevValue
--         elapsedTicks   = realToFrac elapsedTime * ticksPerSecond
--         elapsedTime    = now `Time.diffUTCTime` ctcPrevStamp
--         ticksPerSecond = 100 :: Float
--         -- ^ 100Hz is a constant on Linux, for practical purposes.

zeroUTCTime :: UTCTime
zeroUTCTime = Time.posixSecondsToUTCTime $ realToFrac (0 :: Int)

-- * Basic accumulators
--

-- | Just store the latest value.
mkAccumNew :: Accum Word64 Word64
mkAccumNew = mkAccum 0 0 $
  \_dt _old new -> new

-- | Simply compute the increase.
mkAccumDelta :: Accum Word64 Word64
mkAccumDelta = mkAccum 0 0 $
  \_dt old new -> new - old

-- | Interpret values as centiseconds (ticks, in Linux),
--   and compute ratio of elapsed time, in percents.
mkAccumTicksShare :: Accum Word64 Word64
mkAccumTicksShare = mkAccum 0 0 $
  \dt old new ->
    let spentTicks     = new - old
        elapsedTicks   = realToFrac dt * ticksPerSecond
        ticksPerSecond = 100 :: Float
    in ceiling $ fromIntegral (spentTicks * 100) / elapsedTicks
