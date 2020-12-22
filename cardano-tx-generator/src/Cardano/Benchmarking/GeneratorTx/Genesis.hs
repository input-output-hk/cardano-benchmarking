{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Benchmarking.GeneratorTx.Genesis
  ( GeneratorFunds(..)
  , parseGeneratorFunds
  , genesisFundForKey
  , genesisExpenditure
  , keyAddress
  )
where

import           Cardano.Prelude hiding (TypeError, filter)
import           Prelude (error, filter)
import qualified Data.Map.Strict as Map

import qualified Options.Applicative as Opt

import           Control.Arrow ((***))
import           Cardano.Api.Typed
import           Cardano.Api.Shelley (fromShelleyLovelace, fromShelleyStakeReference, fromShelleyPaymentCredential)
import           Cardano.CLI.Types

import           Cardano.Benchmarking.GeneratorTx.Tx
import           Cardano.Benchmarking.GeneratorTx.CLI.Parsers

import           Shelley.Spec.Ledger.API (ShelleyGenesis, sgInitialFunds)

data GeneratorFunds
  = FundsGenesis   SigningKeyFile
  | FundsUtxo      SigningKeyFile TxIn (TxOut ShelleyEra)
  | FundsSplitUtxo SigningKeyFile FilePath
  deriving stock Show

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

keyAddress :: forall era. IsShelleyBasedEra era => NetworkId -> SigningKey PaymentKey -> AddressInEra era
keyAddress networkId k
  = makeShelleyAddressInEra
      networkId
      (PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey k)
      NoStakeAddress


genesisFunds :: forall era. IsShelleyBasedEra era
  => NetworkId -> ShelleyGenesis StandardShelley -> [(AddressInEra era, Lovelace)]
genesisFunds networkId g =
    map (castAddr *** fromShelleyLovelace)
     $ Map.toList
     $ sgInitialFunds g
  where
    castAddr (Addr _ pcr stref)
      = shelleyAddressInEra $ makeShelleyAddress networkId (fromShelleyPaymentCredential pcr) (fromShelleyStakeReference stref)
    castAddr _ = error "castAddr:  unhandled Shelley.Addr case"


genesisFundForKey :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> ShelleyGenesis StandardShelley
  -> SigningKey PaymentKey
  -> (AddressInEra era, Lovelace)
genesisFundForKey networkId genesis key =
    fromMaybe (error "No genesis funds for signing key.")
  . head
  . filter (isTxOutForKey . fst)
  $ genesisFunds networkId genesis
  where
    isTxOutForKey addr = keyAddress networkId key == addr


genesisExpenditure ::
     IsShelleyBasedEra era
  => NetworkId
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> Lovelace
  -> Lovelace
  -> SlotNo
  -> (Tx era, TxIn, TxOut era)
genesisExpenditure networkId key addr coin fee ttl =  (tx, fundOut, txout )
 where
   tx = mkGenTransaction (castKey key) 0 ttl fee [ pseudoTxIn ] [ txout ]

   txout = TxOut addr $ mkTxOutValueAdaOnly $ coin - fee

   pseudoTxIn = genesisUTxOPseudoTxIn networkId
                  (verificationKeyHash $ getVerificationKey $ castKey key)

   castKey :: SigningKey PaymentKey -> SigningKey GenesisUTxOKey
   castKey(PaymentSigningKey skey) = GenesisUTxOSigningKey skey

   fundOut = TxIn (getTxId $ getTxBody tx) (TxIx 0)
