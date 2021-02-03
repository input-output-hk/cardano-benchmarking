{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Benchmarking.GeneratorTx.Tx
  (
    Fund
  , mkFund
  , fundTxIn
  , fundAdaValue
  , keyAddress
  , mkGenesisTransaction -- ? needed ??
  , mkTransactionGen
  , mkTxOutValueAdaOnly
  , txOutValueToLovelace
  , mkFee
  , mkValidityUpperBound
  )
where

import           Prelude

import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import           Cardano.Benchmarking.GeneratorTx.Benchmark (TxAdditionalSize(..))

import           Cardano.Api


type Fund = (TxIn, InAnyCardanoEra TxOutValue)

mkFund :: forall era. IsCardanoEra era => TxIn -> TxOutValue era -> Fund
mkFund txIn val = (txIn, InAnyCardanoEra cardanoEra val)

fundTxIn :: Fund -> TxIn
fundTxIn (x,_) = x

fundAdaValue :: Fund -> Lovelace
fundAdaValue (_, InAnyCardanoEra _ txOut) = txOutValueToLovelace txOut

keyAddress :: forall era. IsShelleyBasedEra era => NetworkId -> SigningKey PaymentKey -> AddressInEra era
keyAddress networkId k
  = makeShelleyAddressInEra
      networkId
      (PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey k)
      NoStakeAddress

{-# DEPRECATED mkGenesisTransaction "to be removed" #-}
mkGenesisTransaction :: forall era .
     IsShelleyBasedEra era
  => SigningKey GenesisUTxOKey
  -> TxAdditionalSize
  -> SlotNo
  -> Lovelace
  -> [TxIn]
  -> [TxOut era]
  -> Tx era
mkGenesisTransaction key _payloadSize ttl fee txins txouts
  = case makeTransactionBody txBodyContent of
    Right b -> signShelleyTransaction b [WitnessGenesisUTxOKey key]
    Left err -> error $ show err
  where
    txBodyContent = TxBodyContent {
        txIns = txins
      , txOuts = txouts
      , txFee = fees
      , txValidityRange = (TxValidityNoLowerBound, validityUpperBound)
      , txMetadata = TxMetadataNone
      , txAuxScripts = TxAuxScriptsNone
      , txWithdrawals = TxWithdrawalsNone
      , txCertificates = TxCertificatesNone
      , txUpdateProposal = TxUpdateProposalNone
      , txMintValue = TxMintNone
      }
    fees = case shelleyBasedEra @ era of
      ShelleyBasedEraShelley -> TxFeeExplicit TxFeesExplicitInShelleyEra fee
      ShelleyBasedEraAllegra -> TxFeeExplicit TxFeesExplicitInAllegraEra fee
      ShelleyBasedEraMary    -> TxFeeExplicit TxFeesExplicitInMaryEra fee

    validityUpperBound = case shelleyBasedEra @ era of
      ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
      ShelleyBasedEraAllegra -> TxValidityUpperBound ValidityUpperBoundInAllegraEra ttl
      ShelleyBasedEraMary    -> TxValidityUpperBound ValidityUpperBoundInMaryEra ttl

mkTransaction :: forall era .
     IsShelleyBasedEra era
  => SigningKey PaymentKey
  -> TxMetadataInEra era
  -> SlotNo
  -> Lovelace
  -> [TxIn]
  -> [TxOut era]
  -> Tx era
mkTransaction key metadata ttl fee txins txouts
  = case makeTransactionBody txBodyContent of
    Right b -> signShelleyTransaction b [WitnessPaymentKey key]
    Left err -> error $ show err
  where
    txBodyContent = TxBodyContent {
        txIns = txins
      , txOuts = txouts
      , txFee = mkFee fee
      , txValidityRange = (TxValidityNoLowerBound, mkValidityUpperBound ttl)
      , txMetadata = metadata
      , txAuxScripts = TxAuxScriptsNone
      , txWithdrawals = TxWithdrawalsNone
      , txCertificates = TxCertificatesNone
      , txUpdateProposal = TxUpdateProposalNone
      , txMintValue = TxMintNone
      }

mkFee :: forall era .
     IsShelleyBasedEra era
  => Lovelace
  -> TxFee era
mkFee f = case shelleyBasedEra @ era of
  ShelleyBasedEraShelley -> TxFeeExplicit TxFeesExplicitInShelleyEra f
  ShelleyBasedEraAllegra -> TxFeeExplicit TxFeesExplicitInAllegraEra f
  ShelleyBasedEraMary    -> TxFeeExplicit TxFeesExplicitInMaryEra f

mkValidityUpperBound :: forall era .
     IsShelleyBasedEra era
  => SlotNo
  -> TxValidityUpperBound era
mkValidityUpperBound ttl = case shelleyBasedEra @ era of
  ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
  ShelleyBasedEraAllegra -> TxValidityUpperBound ValidityUpperBoundInAllegraEra ttl
  ShelleyBasedEraMary    -> TxValidityUpperBound ValidityUpperBoundInMaryEra ttl

-- this is basically a transaction with automatic change handling ?

mkTransactionGen :: forall era .
     IsShelleyBasedEra era
  => SigningKey PaymentKey
  -> NonEmpty Fund
  -- ^ Non-empty list of (TxIn, TxOut) that will be used as
  -- inputs and the key to spend the associated value
  -> AddressInEra era
  -> [(Int, TxOut era)]
  -- ^ Each recipient and their payment details
  -> TxMetadataInEra era
  -- ^ Optional size of additional binary blob in transaction (as 'txAttributes')
  -> Lovelace
  -- ^ Tx fee.
  -> ( Maybe (TxIx, Lovelace)   -- The 'change' index and value (if any)
     , Lovelace                 -- The associated fees
     , Map Int TxIx             -- The offset map in the transaction below
     , Tx era
     )
mkTransactionGen signingKey inputs address payments metadata fee =
  (mChange, fee, offsetMap, tx)
 where
  tx = mkTransaction signingKey metadata (SlotNo 10000000)
         fee
         (NonEmpty.toList $ fundTxIn <$> inputs)
         (NonEmpty.toList txOutputs)

  payTxOuts     = map snd payments

  totalInpValue = sum $ fundAdaValue <$> inputs
  totalOutValue = txOutSum payTxOuts
  changeValue = totalInpValue - totalOutValue - fee
      -- change the order of comparisons first check emptyness of txouts AND remove appendr after

  (txOutputs, mChange) =
    if changeValue > 0
    then
      let changeTxOut   = TxOut address $ mkTxOutValueAdaOnly changeValue
          changeIndex   = TxIx $ fromIntegral $ length payTxOuts -- 0-based index
      in
          (appendr payTxOuts (changeTxOut :| []), Just (changeIndex, changeValue))
    else
      case payTxOuts of
        []                 -> error "change is zero and txouts is empty"
        txout0: txoutsRest -> (txout0 :| txoutsRest, Nothing)

  -- TxOuts of recipients are placed at the first positions
  offsetMap = Map.fromList $ zipWith (\payment index -> (fst payment, TxIx index))
                                     payments
                                     [0..]
  txOutSum :: [(TxOut era)] -> Lovelace
  txOutSum l = sum $ map toVal l

  toVal (TxOut _ val) = txOutValueToLovelace val

  -- | Append a non-empty list to a list.
  -- > appendr [1,2,3] (4 :| [5]) == 1 :| [2,3,4,5]
  appendr :: [a] -> NonEmpty a -> NonEmpty a
  appendr l nel = foldr NonEmpty.cons nel l

mkTxOutValueAdaOnly :: forall era . IsShelleyBasedEra era => Lovelace -> TxOutValue era
mkTxOutValueAdaOnly l = case shelleyBasedEra @ era of
  ShelleyBasedEraShelley -> TxOutAdaOnly AdaOnlyInShelleyEra l
  ShelleyBasedEraAllegra -> TxOutAdaOnly AdaOnlyInAllegraEra l
  ShelleyBasedEraMary    -> TxOutValue MultiAssetInMaryEra $ lovelaceToValue l

txOutValueToLovelace :: TxOutValue era -> Lovelace
txOutValueToLovelace = \case
  TxOutAdaOnly AdaOnlyInByronEra   x -> x
  TxOutAdaOnly AdaOnlyInShelleyEra x -> x
  TxOutAdaOnly AdaOnlyInAllegraEra x -> x
  TxOutValue _ v -> case valueToLovelace v of
    Just c -> c
    Nothing -> error "txOutValueLovelace  TxOut contains no ADA"
