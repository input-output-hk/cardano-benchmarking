module Cardano.BM.Adopted
  (
      Adopted (..)
    , parseline
  )
where

import Data.Text.Lazy (Text, unpack)

import Cardano.BM.Common

data Adopted = Adopted {
     node :: NodeId,
     slot :: SlotNum,
     timestamp :: Timestamp,
     msg :: Text,
     blockhash :: Text,
     ntx :: Int
  } deriving (Show)

instance Lineparser Adopted where
    -- example:
    -- 0,"21","2020-05-13 08:06:45.01","TraceAddBlockEvent.AddedToCurrentChain"

    itemFromArray [n, sn, ts, m, bh, numtx] = Adopted
                                    (read . unpack $ n)
                                    (read . unpack $ sn)
                                    (parseTS (remquotes ts))
                                    (remquotes m)
                                    (remquotes bh)
                                    (read . unpack $ numtx)
    itemFromArray _ = Adopted (-1) (-1) time0 "error" "" 0