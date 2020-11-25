{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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
  ( GeneratorFunds(..)
  , parseGeneratorFunds
  , extractGenesisFunds
  , genesisExpenditure
  , keyAddress
  )
where

import           Cardano.Prelude hiding (TypeError)
import           Prelude (error)

import           Control.Arrow ((***))
import qualified Data.Map.Strict as Map
import qualified Options.Applicative as Opt

-- Era-agnostic imports
import           Cardano.Api.Typed
import           Cardano.CLI.Types
                   (SigningKeyFile(..))
import qualified Ouroboros.Consensus.Cardano as Consensus

-- Byron-specific imports
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron

-- Shelley-specific imports
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

-- Local imports
import           Cardano.Benchmarking.GeneratorTx.Era
import           Cardano.Benchmarking.GeneratorTx.Tx
import           Cardano.Benchmarking.GeneratorTx.Tx.Byron
import           Cardano.Benchmarking.GeneratorTx.CLI.Parsers


data GeneratorFunds
  = FundsGenesis   SigningKeyFile
  | FundsUtxo      SigningKeyFile TxIn (TxOut Shelley)
  | FundsSplitUtxo SigningKeyFile FilePath
  deriving Show

parseGeneratorFunds :: Opt.Parser GeneratorFunds
parseGeneratorFunds =
  (FundsGenesis
    <$> parseSigningKeysFile
        "genesis-funds-key"
        "Genesis UTxO funds signing key.")
  <|>
  (FundsUtxo
    <$> parseSigningKeysFile
        "utxo-funds-key"
        "UTxO funds signing key."
    <*> pTxIn
    <*> pTxOut)
  <|>
  (FundsSplitUtxo
    <$> parseSigningKeysFile
        "split-utxo-funds-key"
        "UTxO funds signing key."
    <*> parseFilePath
        "split-utxo"
        "UTxO funds file.")

keyAddress :: Mode mode era -> (Mode mode era -> NetworkId) -> SigningKeyOf era -> AddressInEra era
keyAddress m toNetId = case modeEra m of
  EraByron{}   -> \(getVerificationKey -> ByronVerificationKey k) ->
    AddressInEra ByronAddressInAnyEra $
      ByronAddress
        (Byron.makeVerKeyAddress (toByronNetworkMagic $ toNetId m) k)
  EraShelley{} -> \k ->
    makeShelleyAddressInEra
      (toNetId m)
      (PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey k)
      NoStakeAddress

-- https://github.com/input-output-hk/cardano-node/issues/1856 would be the proper solution.
genesisKeyPseudoTxIn :: Mode mode era -> SigningKeyOf era -> AddressInEra era -> TxIn
genesisKeyPseudoTxIn m@ModeShelley{} key _ =
  genesisUTxOPseudoTxIn
    (modeNetworkId m)
    (verificationKeyHash $ getVerificationKey $ castSigningKeyRolePaymentKeyGenesisUTxOKey key)
 where
   castSigningKeyRolePaymentKeyGenesisUTxOKey ::
     SigningKey PaymentKey -> SigningKey GenesisUTxOKey
   castSigningKeyRolePaymentKeyGenesisUTxOKey (PaymentSigningKey skey) =
     GenesisUTxOSigningKey skey
genesisKeyPseudoTxIn m@ModeByron{}
                     (getVerificationKey -> ByronVerificationKey key)
                     (AddressInEra _ (ByronAddress genAddr)) =
  fromByronTxIn $ byronGenesisUTxOTxIn (modeGenesis m) key genAddr
genesisKeyPseudoTxIn m _ _ =
  error $ "genesisKeyPseudoTxIn:  unsupported mode: " <> show m

-- https://github.com/input-output-hk/cardano-node/issues/1861 would be the proper solution.
modeGenesisFunds :: Mode mode era
                   -> [(AddressInEra era, Lovelace)]
modeGenesisFunds = \case
  m@ModeShelley{} ->
    fmap (fromShelleyAddr m *** fromShelleyLovelace)
    . Map.toList
    . Consensus.sgInitialFunds
    $ modeGenesis m
  m@ModeByron{} ->
    fmap (\(TxOut addr (TxOutAdaOnly AdaOnlyInByronEra coin)) -> (addr, coin))
    . map (fromByronTxOut . Byron.fromCompactTxOut . snd)
    . Map.toList
    . Byron.unUTxO
    . Byron.genesisUtxo
    $ modeGenesis m
  m -> error $ "modeGenesisFunds:  unsupported mode: " <> show m

extractGenesisFunds
  :: forall mode era
  .  Eq (AddressInEra era)
  => Mode mode era
  -> SigningKeyOf era
  -> (TxIn, TxOut era)
extractGenesisFunds m k =
    fromMaybe (error "No genesis funds for signing key.")
  . head
  . filter (isTxOutForKey . snd)
  . fmap genesisFundsEntryTxIO
  . modeGenesisFunds
  $ m
 where
  genesisFundsEntryTxIO :: (AddressInEra era, Lovelace) -> (TxIn, TxOut era)
  genesisFundsEntryTxIO (addr, coin) =
    (genesisKeyPseudoTxIn m k addr, TxOut addr (mkTxOutValueAdaOnly (modeEra m) coin))

  isTxOutForKey :: TxOut era -> Bool
  isTxOutForKey (TxOut addr _) = keyAddress m modeNetworkId k == addr

genesisExpenditure :: Mode mode era -> SigningKeyOf era -> AddressInEra era -> Lovelace -> TxFee -> TTL -> (Tx era, TxIn, TxOut era)
genesisExpenditure m key addr (Lovelace coin) (Lovelace fee) ttl =
  (,,) tx txin txout
 where
   tx = mkTransaction m key 0 ttl (Lovelace fee)
          [genesisKeyPseudoTxIn m key (keyAddress m modeNetworkId key)]
          [txout]
   txin = TxIn (getTxId $ getTxBody tx) (TxIx 0)
   txout = TxOut addr (mkTxOutValueAdaOnly (modeEra m) (Lovelace (coin - fee)))

