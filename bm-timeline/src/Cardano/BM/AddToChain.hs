module Cardano.BM.AddToChain
  (
      AddToChain (..)
    , parseline
  )
where

import           Data.Text.Lazy (Text, unpack)

import           Cardano.BM.Common

data AddToChain = AddToChain
    { node      :: NodeId
    , slot      :: SlotNum
    , timestamp :: Timestamp
    , msg       :: Text
    }
    deriving (Show)

instance Lineparser AddToChain where
    -- example:
    -- 0,"21","2020-05-13 08:06:45.01","TraceAddBlockEvent.AddedToCurrentChain"

    itemFromArray [n, sn, ts, m] = AddToChain
                                    (read . unpack $ n)
                                    (read . unpack $ remquotes sn)
                                    (parseTS (remquotes ts))
                                    (remquotes m)
    itemFromArray _ = AddToChain (-1) (-1) time0 "error"
