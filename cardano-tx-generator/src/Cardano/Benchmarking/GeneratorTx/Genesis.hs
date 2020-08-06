{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Benchmarking.GeneratorTx.Genesis
  ( extractGenesisFunds
  , genesisExpenditure
  , keyAddress
  )
where

import           Cardano.Prelude hiding (TypeError)
import           Prelude (error)

import           Control.Arrow ((***))
import qualified Data.Map.Strict as Map

-- Era-agnostic imports
import qualified Ouroboros.Consensus.Cardano as Consensus

-- Byron-specific imports
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron

-- Shelley-specific imports
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Shelley

import           Cardano.Api.Typed
import           Cardano.Benchmarking.GeneratorTx.Era
import           Cardano.Benchmarking.GeneratorTx.Tx
import           Cardano.Benchmarking.GeneratorTx.Tx.Byron


keyAddress :: Era era -> SigningKeyOf era -> Address era
keyAddress p@EraByron{} (getVerificationKey -> ByronVerificationKey k) =
  ByronAddress $ Byron.makeVerKeyAddress (toByronNetworkMagic $ eraNetworkId p) k
keyAddress p@EraShelley{} k =
  makeShelleyAddress
    (eraNetworkId p)
    (PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey k)
    NoStakeAddress

genesisKeyPseudoTxIn :: Era era -> SigningKeyOf era -> Address era -> TxIn
genesisKeyPseudoTxIn p@EraShelley{} key _ =
  genesisUTxOPseudoTxIn
    (eraNetworkId p)
    (verificationKeyHash $ getVerificationKey $ castSigningKeyRolePaymentKeyGenesisUTxOKey key)
 where
   castSigningKeyRolePaymentKeyGenesisUTxOKey ::
     SigningKey PaymentKey -> SigningKey GenesisUTxOKey
   castSigningKeyRolePaymentKeyGenesisUTxOKey (PaymentSigningKey skey) =
     GenesisUTxOSigningKey skey
genesisKeyPseudoTxIn p@EraByron{}
                     (getVerificationKey -> ByronVerificationKey key)
                     (ByronAddress genAddr) =
  fromByronTxIn $ byronGenesisUTxOTxIn (eraLedgerConfig p) key genAddr

eraGenesisFunds :: Era era
                   -> [(Address era, Lovelace)]
eraGenesisFunds p@EraShelley{} =
    fmap (fromShelleyAddr *** fromShelleyLovelace)
  . Map.toList
  . Consensus.sgInitialFunds
  . Shelley.shelleyLedgerGenesis
  $ eraLedgerConfig p
eraGenesisFunds p@EraByron{} =
    fmap (\(TxOut addr coin) -> (addr, coin))
  . map (fromByronTxOut . Byron.fromCompactTxOut . snd)
  . Map.toList
  . Byron.unUTxO
  . Byron.genesisUtxo
  $ eraLedgerConfig p

extractGenesisFunds
  :: forall era
  .  Eq (Address era)
  => Era era
  -> SigningKeyOf era
  -> (TxIn, TxOut era)
extractGenesisFunds p k =
    fromMaybe (error "No genesis funds for signing key.")
  . head
  . filter (isTxOutForKey . snd)
  . fmap genesisFundsEntryTxIO
  . eraGenesisFunds
  $ p
 where
  genesisFundsEntryTxIO :: (Address era, Lovelace) -> (TxIn, TxOut era)
  genesisFundsEntryTxIO (addr, coin) =
    (genesisKeyPseudoTxIn p k addr, TxOut addr coin)

  isTxOutForKey :: TxOut era -> Bool
  isTxOutForKey (TxOut addr _) = keyAddress p k == addr

genesisExpenditure :: Era era -> SigningKeyOf era -> Address era -> Lovelace -> TxFee -> TTL -> (Tx era, TxIn, TxOut era)
genesisExpenditure p key addr (Lovelace coin) (Lovelace fee) ttl =
  (,,) tx txin txout
 where
   tx = mkTransaction p key 0 ttl (Lovelace fee)
          [genesisKeyPseudoTxIn p key (keyAddress p key)]
          [txout]
   txin = TxIn (getTxId $ getTxBody tx) (TxIx 0)
   txout = TxOut addr (Lovelace (coin - fee))
