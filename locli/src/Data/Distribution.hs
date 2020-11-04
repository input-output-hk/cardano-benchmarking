{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Distribution
  ( ToRealFrac(..)
  , Distribution(..)
  , computeDistribution
  , zeroDistribution
  , spans
  , PercSpec(..)
  , renderPercSpec
  , Percentile(..)
  , pctFrac
  , psNamedAbove
  ) where

import           Prelude (String)
import           Cardano.Prelude

import           Data.Aeson (ToJSON(..))
import qualified Data.Foldable as F
import           Data.List (span)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, index)
import           Text.Printf (PrintfArg, printf)

data Distribution a b =
  Distribution
  { dAverage      :: a
  , dCount        :: Int
  , dPercentiles  :: [Percentile a b]
  }
  deriving (Generic, Show)

instance (ToJSON a, ToJSON b) => ToJSON (Distribution a b)

data PercSpec a
  = PercAnon  { psFrac :: !a }
  | PercNamed { psName :: !String, psFrac :: !a }
  deriving (Generic, Show)

psNamedAbove :: Ord a => String -> a -> [a] -> PercSpec Float
psNamedAbove name thresh xs = PercNamed name . (1.0 -) $
  fromIntegral (length (takeWhile (>= thresh) sorted)) / fromIntegral size
 where sorted = sortBy (\x y -> compare y x) xs
       size = length sorted

renderPercSpec :: PrintfArg a => Int -> PercSpec a -> String
renderPercSpec width = \case
  PercAnon x    -> printf ("%0."<>show (width-2)<>"f") x
  PercNamed n _ -> n

data Percentile a b =
  Percentile
  { pctSpec        :: !(PercSpec a)
  , pctSampleIndex :: !Int
  , pctSamplePrev  :: !Int
  , pctSample      :: !b
  }
  deriving (Generic, Show)

pctFrac :: Percentile a b -> a
pctFrac = psFrac . pctSpec

instance (ToJSON a) => ToJSON (PercSpec a)
instance (ToJSON a, ToJSON b) => ToJSON (Percentile a b)

zeroDistribution :: Num a => Distribution a b
zeroDistribution =
  Distribution
  { dAverage     = 0
  , dCount       = 0
  , dPercentiles = mempty
  }

spans :: forall a. (a -> Bool) -> [a] -> [[a]]
spans f = go []
 where
   go :: [[a]] -> [a] -> [[a]]
   go acc [] = reverse acc
   go acc xs =
     case span f $ dropWhile (not . f) xs of
       ([], rest) -> go acc rest
       (ac, rest) -> go (ac:acc) rest


countSeq :: Eq a => a -> Seq a -> Int
countSeq x = foldl' (\n e -> if e == x then n + 1 else n) 0

computeDistribution :: (RealFrac a, Real v, ToRealFrac v a) => [PercSpec a] -> Seq v -> Distribution a v
computeDistribution _ Seq.Empty = Distribution 0 0 []
computeDistribution percentiles (Seq.sort -> sorted) =
  Distribution
  { dAverage     = toRealFrac (F.sum sorted) / fromIntegral size
  , dCount       = size
  , dPercentiles =
    (Percentile     (PercAnon 0)   size (countSeq mini sorted) mini:) .
    (<> [Percentile (PercAnon 1.0) 1    (countSeq maxi sorted) maxi]) $
    percentiles <&>
      \spec ->
        let sample = Seq.index sorted sampleIndex
            sampleIndex :: Int = floor $ fromIntegral (size - 1) * psFrac spec
        in Percentile
             spec
             (size - sampleIndex)
             (countSeq sample sorted)
             sample
  }
  where size   = Seq.length sorted
        (,) mini maxi = (index sorted 0, index sorted $ size - 1)

class RealFrac b => ToRealFrac a b where
  toRealFrac :: a -> b

instance RealFrac b => ToRealFrac Int b where
  toRealFrac = fromIntegral

instance {-# OVERLAPPABLE #-} (RealFrac b, Real a) => ToRealFrac a b where
  toRealFrac = realToFrac