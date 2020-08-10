module Cardano.BM.TxMempool
  (
      TxMempool (..)
    , parseline
  )
where

import           Data.Text.Lazy (Text, unpack)

import           Cardano.BM.Common

data TxMempool = TxMempool
    { txid      :: Text
    , timestamp :: Timestamp
    , node      :: NodeId
    }
    deriving (Show)

instance Lineparser TxMempool where
    -- example:
    -- "1add2e1f71d1bbe2999bcafca48cafe39045f6a31358ab5f0131e6233e708cb7","2020-06-22 08:33:10.67","0","TraceMempoolAddedTx"

    itemFromArray [tx, ts, n, _ev] = TxMempool
                                    (remquotes tx)
                                    (parseTS (remquotes ts))
                                    (read . unpack $ n)
    itemFromArray _ = TxMempool "n/a" time0 (-1)

