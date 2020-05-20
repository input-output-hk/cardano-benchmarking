module Cardano.BM.Leader
  (
      Leader (..)
    , parseline
  )
where

import Data.Text.Lazy (Text, unpack)

import Cardano.BM.Common

data Leader = Leader {
     node :: NodeId,
     slot :: SlotNum,
     timestamp :: Timestamp,
     msg :: Text
  } deriving (Show)

instance Lineparser Leader where
    -- example:
    -- 0,21,"2020-05-13 08:06:45.00","nodeIsLeader"

    itemFromArray [n, sn, ts, m] = Leader
                                (read . unpack $ n)
                                (read . unpack $ sn)
                                (parseTS (remquotes ts))
                                (remquotes m)
    itemFromArray _ = Leader (-1) (-1) time0 "error"