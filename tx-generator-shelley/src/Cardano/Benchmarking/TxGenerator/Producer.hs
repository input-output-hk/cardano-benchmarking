{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.TxGenerator.Producer
where

import           Prelude

import           Cardano.Api.Typed (Address, Lovelace (..), NetworkId, PaymentKey, Shelley,
                                    ShelleyWitnessSigningKey (WitnessPaymentKey), SigningKey (..),
                                    TTL, Tx, TxBody, TxIn (..), TxIx (..), TxOut (..), getTxId,
                                    makeShelleyKeyWitness, makeShelleyTransaction,
                                    makeSignedTransaction, txExtraContentEmpty)

deriving instance Num Lovelace

{-
It may be possible to use a fancy co-monad here.
-}
data Producer = Producer
    { network :: !NetworkId
    , ttl     :: !TTL
    , fee     :: !Lovelace
    , addr    :: !(Address Shelley)
    , skey    :: !(SigningKey PaymentKey)
    , src     :: !TxIn
    , fund    :: !Lovelace
    -- deriving (Show)
    }

data PError = UnsufficientFunds
    deriving (Show, Eq)

extract :: Producer -> (TxIn, Lovelace)
extract (Producer {src, fund}) = (src, fund)

setFunds :: Producer -> TxIn -> Lovelace -> Producer
setFunds p txIn ada = p { src = txIn, fund = ada}

fullTransfer :: Producer -> Either PError (Tx Shelley, Producer)
fullTransfer p@(Producer {..})
  = if fee > fund
    then Left UnsufficientFunds
    else Right (txSigned, setFunds p newSrc newVal)
  where
    newVal = fund - fee
    tx = makeShelleyTransaction
      txExtraContentEmpty
      ttl
      fee
      [ src ]
      [ TxOut addr newVal ]
    newSrc = TxIn (getTxId tx) $ TxIx 0
    txSigned = signTransaction tx skey

payWithChange :: Lovelace -> Producer -> Either PError (Tx Shelley, Producer, Producer)
payWithChange payment p@(Producer {..})
  = if fund < payment + fee
      then Left UnsufficientFunds
      else Right (txSigned, p0, p1)
  where
    change = fund - payment - fee
    tx = makeShelleyTransaction
      txExtraContentEmpty
      ttl
      fee
      [ src ]
      [ TxOut addr payment , TxOut addr change ]
    txSigned = signTransaction tx skey
    txId = getTxId tx
    p0 = setFunds p (TxIn txId $ TxIx 0) payment
    p1 = setFunds p (TxIn txId $ TxIx 1) change

payWithChangeSeq :: [Lovelace] -> Producer -> Either PError ([Tx Shelley], [Producer], Producer)
payWithChangeSeq []  p = Right ([], [], p)
payWithChangeSeq (ada:l) p = do
  (tx, out, rest ) <- payWithChange ada p
  (txs, outs, change) <- payWithChangeSeq l rest
  return (tx:txs, out:outs, change)

split :: [Lovelace] -> Producer -> Either PError (Tx Shelley, [Producer])
split outValues p@(Producer {..})
  = if fund < fee + sum outValues
      then Left UnsufficientFunds
      else Right (txSigned, outs)
  where
    tx = makeShelleyTransaction
      txExtraContentEmpty
      ttl
      fee
      [ src ]
      (map (\l -> TxOut addr l) outValues)
    txSigned = signTransaction tx skey
    txId = getTxId tx
    outs = map (\(ada, ix) -> setFunds p (TxIn txId ix) ada)
               $ zip outValues [TxIx 0..]

signTransaction :: TxBody Shelley -> SigningKey PaymentKey -> Tx Shelley
signTransaction tx key
   = makeSignedTransaction [ makeShelleyKeyWitness tx $ WitnessPaymentKey key ] tx
