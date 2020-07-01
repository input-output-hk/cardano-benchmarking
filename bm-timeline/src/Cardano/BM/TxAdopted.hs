module Cardano.BM.TxAdopted
  (
      TxAdopted (..)
    , parseline
  )
where

import Data.Text.Lazy (Text, unpack)

import Cardano.BM.Common

data TxAdopted = TxAdopted {
     txid :: Text,
     timestamp :: Timestamp,
     node :: NodeId,
     slot :: SlotNum
  } deriving (Show)

instance Lineparser TxAdopted where
    -- example:
    -- "1add2e1f71d1bbe2999bcafca48cafe39045f6a31358ab5f0131e6233e708cb7", "2020-06-22 08:33:14.90", 2, 1278

    itemFromArray [tx, ts, n, sn] = TxAdopted
                                    (remquotes tx)
                                    (parseTS (remquotes ts))
                                    (read . unpack $ n)
                                    (read . unpack $ sn)
    itemFromArray _ = TxAdopted "n/a" time0 (-1) (-1)