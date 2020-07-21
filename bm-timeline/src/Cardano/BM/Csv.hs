{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Csv
    (
      output_csv
    , named_columns
    , pairs_in_columns
    , list_to_columns
    , Range
    )
where

import Data.Text (Text, intercalate, pack)
import qualified Data.Text.IO as TIO

import System.IO (Handle)

type ValTy = Text

type Column = [ValTy]
-- type Row = [ValTy]

type Range = [Column]

output_csv :: Handle -> Range -> IO ()
output_csv _h [] = pure ()
output_csv h (r : rs) = do
    TIO.hPutStrLn h $ intercalate "," r
    output_csv h rs

named_columns :: [([Text], Range)] -> Range
named_columns ls =
    enter_ranges ls (0,[[]])
  where
    enter_ranges :: [([Text], Range)] -> (Int, Range) -> Range
    enter_ranges [] (_w,acc) = acc
    enter_ranges ((headers, range):r) (w,acc) = 
      let w2 = length headers
          newcols = headers : range
          rng2 = append_cols (w,acc) (w2,newcols) []
      in enter_ranges r (w+w2, rng2)
    append_cols (_,[]) (_,[]) acc = reverse acc   --termination
    append_cols (w1,[]) (w2,(b : bs)) acc = append_cols (w1,[]) (w2, bs) $ ((replicate w1 (pack " ")) ++ b) : acc
    append_cols (w1,(a : as)) (w2, []) acc = append_cols (w1,as) (w2,[]) $ (a ++ replicate w2 (" "::Text)) : acc
    append_cols (w1,(a : as)) (w2,(b : bs)) acc = append_cols (w1,as) (w2,bs) $ (a ++ b) : acc


pairs_in_columns :: (Show a, Show b) => [(a,b)] -> Range
pairs_in_columns = map (\(a,b) -> [textify a, textify b])

list_to_columns :: Show a => [a] -> Range
list_to_columns ls = [ map textify ls ]

textify :: (Show a) => a -> Text
textify = pack . show