module Cardano.BM.CPUticks
  (
      CPUticks (..)
    , parseline
  )
where

import           Data.Text.Lazy (unpack)

import           Cardano.BM.Common

data CPUticks
  = CPUticks
      { timestamp :: Timestamp
      , ticks     :: Int
      , node      :: NodeId
      }
  deriving (Show)

instance Lineparser CPUticks where
    -- example:
    -- "2020-06-29 16:06:21.85","Stat.cputicks",1252

    itemFromArray [ts, _ev, t] = CPUticks
                                    (parseTS (remquotes ts))
                                    (read . unpack $ t)
                                    (-1)
    itemFromArray _ = CPUticks time0 (-1) (-1)
