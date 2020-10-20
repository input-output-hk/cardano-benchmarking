{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Distribution
  ( ToRealFrac(..)
  , Distribution(..)
  , computeDistribution
  , zeroDistribution
  ) where

import           Cardano.Prelude

import           Control.Arrow ((&&&))
import           Data.Aeson (ToJSON(..))
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, index)

data Distribution a b =
  Distribution
  { dMinimum      :: b
  , dAverage      :: a
  , dMaximum      :: b
  , dPercentiles  :: [(a, b)]
  }
  deriving (Generic, Show)

instance (ToJSON a, ToJSON b) => ToJSON (Distribution a b)

zeroDistribution :: (Num a, Num b) => Distribution a b
zeroDistribution =
  Distribution
  { dMinimum     = 0
  , dAverage     = 0
  , dMaximum     = 0
  , dPercentiles = mempty
  }

computeDistribution :: (RealFrac a, Real v, ToRealFrac v a) => [a] -> Seq v -> Distribution a v
computeDistribution _ Seq.Empty = Distribution 0 0 0 []
computeDistribution percentiles (Seq.sort -> sorted) =
  Distribution
  { dMinimum     = mini
  , dAverage     = toRealFrac (F.sum sorted) / fromIntegral size
  , dMaximum     = maxi
  , dPercentiles = ((0, mini):) . (<> [(1.0, maxi)]) $
    percentiles <&>
      identity &&& Seq.index sorted . floor . (fromIntegral size *)
  }
  where size   = Seq.length sorted
        (,) mini maxi = (index sorted 0, index sorted $ size - 1)

class RealFrac b => ToRealFrac a b where
  toRealFrac :: a -> b

instance RealFrac b => ToRealFrac Int b where
  toRealFrac = fromIntegral

instance {-# OVERLAPPABLE #-} (RealFrac b, Real a) => ToRealFrac a b where
  toRealFrac = realToFrac
