{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Cardano.Benchmarking.TxGenerator.Producer
where

import           Prelude
import qualified Data.Map as Map

import           Cardano.Api
import           Shelley.Spec.Ledger.TxData as Shelley (Wdrl(..))

deriving instance Num Lovelace
deriving instance Ord Lovelace

{-
It may be possible to use a fancy co-monad here.
-}
data Producer = Producer
  { network :: !Network
  , ttl     :: !SlotNo
  , fee     :: !Lovelace
  , addr    :: !Address
  , skey    :: !SigningKey
  , src     :: !TxIn
  , fund    :: !Lovelace
  } deriving (Show)

data PError = UnsufficientFunds
  deriving (Show,Eq)

extract :: Producer -> (TxIn, Lovelace)
extract (Producer {src, fund}) = (src, fund)

setFunds :: Producer -> TxIn -> Lovelace -> Producer
setFunds p txIn ada = p { src = txIn, fund = ada}

fullTransfer :: Producer -> Either PError (TxSigned, Producer)
fullTransfer p@(Producer {..})
  = if fee > fund
    then Left UnsufficientFunds
    else Right (txSigned, setFunds p newSrc newVal)
  where
    newVal = fund - fee
    tx = buildTX
      [ src ]
      [ TxOut addr newVal ]
      ttl
      fee
    newSrc = TxIn (getTransactionId tx) 0
    txSigned = signTransaction tx network [skey]

payWithChange :: Lovelace -> Producer -> Either PError (TxSigned, Producer, Producer)
payWithChange payment p@(Producer {..})
  = if fund < payment + fee
      then Left UnsufficientFunds
      else Right (txSigned, p0, p1)
  where
    change = fund - payment - fee
    tx = buildTX
      [ src ]
      [ TxOut addr payment , TxOut addr change ]
      ttl
      fee
    txSigned = signTransaction tx network [skey]
    txId = getTransactionId tx
    p0 = setFunds p (TxIn txId 0) payment
    p1 = setFunds p (TxIn txId 1) change

payWithChangeSeq :: [Lovelace] -> Producer -> Either PError ([TxSigned], [Producer], Producer)
payWithChangeSeq []  p = Right ([], [], p)
payWithChangeSeq (ada:l) p = do
  (tx, out, rest ) <- payWithChange ada p
  (txs, outs, change) <- payWithChangeSeq l rest
  return (tx:txs, out:outs, change)

split :: [Lovelace] -> Producer -> Either PError (TxSigned, [Producer] )
split outValues p@(Producer {..})
  = if fund < fee + sum outValues
      then Left UnsufficientFunds
      else Right (txSigned, outs)
  where
    tx = buildTX
      [ src ]
      (map (\l -> TxOut addr l) outValues)
      ttl
      fee
    txSigned = signTransaction tx network [skey]
    txId = getTransactionId tx
    outs = map (\(ada, ix) -> setFunds p (TxIn txId ix) ada)
               $ zip outValues [0..]

buildTX :: [TxIn] -> [TxOut] -> SlotNo -> Lovelace -> TxUnsigned
buildTX src dest ttl fee
  = buildShelleyTransaction src dest ttl fee [] emptyWithdrawl Nothing Nothing
  where
    emptyWithdrawl = WithdrawalsShelley $ Shelley.Wdrl Map.empty

