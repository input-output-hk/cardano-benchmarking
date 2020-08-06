{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Stats
    (
      calc_cdf
    , calc_boxplot
    )
where

import           Data.List (sort)
import           GHC.Float (int2Double)


calc_cdf :: (Ord a, Integral a) => [a] -> [(a, Double)]
calc_cdf [] = []
calc_cdf tms =
    let s_tms = sort tms
        min_tm :: Double = fromIntegral (head s_tms)
        max_tm :: Double = fromIntegral (last s_tms)
    in map (calc_fraction min_tm max_tm s_tms) [1::Double .. 100]
  where
    calc_fraction min_tm max_tm ls q =
        let v = round $ (max_tm - min_tm) * q / 100.0 + min_tm
        in (v, fraction ls v)
    -- | count elements in sorted list smaller or equal than value
    --   TODO: speedup if consecutive calls only count in remaining list
    fraction :: Ord a => [a] -> a -> Double
    fraction ls val =
        let len = length ls
            count = counting ls 0
        in int2Double count / int2Double len
      where
        counting [] c = c
        counting (v : r) c
            | v <= val = counting r (c + 1)
            | otherwise = c

calc_boxplot :: (Ord a, Num a) => [a] -> [a]
calc_boxplot [] = []
calc_boxplot [a] = replicate 5 a
calc_boxplot tms =
    let s_tms = sort tms
        -- len = length s_tms
        min_tm = head s_tms
        max_tm = last s_tms
        q1 = quartile s_tms 0.25
        q2 = quartile s_tms 0.50  -- median
        q3 = quartile s_tms 0.75
    in [min_tm, q1, q2, q3, max_tm]
  where
    quartile :: (Num a, Ord a) => [a] -> Double -> a
    quartile [] _ = -0
    quartile ls frac
        | frac > 0 && frac <= 1.0 =
            let len :: Double = fromIntegral (length ls)
                idx = round (len * frac)
            in ls !! idx
        | otherwise = -0
