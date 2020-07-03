module Cardano.BM.MemResident
  (
      MemResident (..)
    , parseline
  )
where

import Data.Text.Lazy (unpack)

import Cardano.BM.Common

data MemResident = MemResident {
     timestamp :: Timestamp,
     memresident :: Int,
     node :: NodeId
  } deriving (Show)

instance Lineparser MemResident where
    -- example:
    -- "2020-06-29 16:07:21.89","Mem.resident",112472064

    itemFromArray [ts, _ev, mem] = MemResident
                                    (parseTS (remquotes ts))
                                    (read . unpack $ mem)
                                    (-1)
    itemFromArray _ = MemResident time0 (-1) (-1)