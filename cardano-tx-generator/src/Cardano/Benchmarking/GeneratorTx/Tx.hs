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
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Benchmarking.GeneratorTx.Tx
  ( castTxMode
  , fromByronTxId
  , fromByronTxIn
  , fromByronTxOut
  , fromShelleyAddr
  , fromShelleyLovelace
  , fromGenTxId
  , mkTransaction
  , mkTransactionGen
  , signTransaction
  , toGenTx
  )
where

import           Cardano.Prelude hiding (TypeError)
import           Prelude (error)
import qualified Prelude

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- Era-agnostic imports
import           Cardano.Binary (Annotated (..), reAnnotate)
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Ouroboros.Consensus.Block.Abstract (SlotNo (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool hiding (TxId)

-- Byron-specific imports
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Hashing as Byron
import qualified Ouroboros.Consensus.Byron.Ledger as Byron hiding (TxId)

-- Shelley-specific imports
import qualified Ouroboros.Consensus.Shelley.Ledger.Mempool as Shelley
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.TxData as ShelleyLedger

import           Cardano.Api.TxSubmit
import           Cardano.Api.Typed
import           Cardano.Benchmarking.GeneratorTx.Era
import           Cardano.Benchmarking.GeneratorTx.Tx.Byron


castTxMode :: Tx era -> TxForMode (ModeOf era)
castTxMode tx@ByronTx{}   = TxForByronMode  tx
castTxMode tx@ShelleyTx{} = TxForShelleyMode tx

toGenTx :: Tx era -> GenTx (BlockOf era)
toGenTx (ShelleyTx tx) = Shelley.mkShelleyTx tx
toGenTx (ByronTx tx)   = normalByronTxToGenTx tx

fromGenTxId :: Era era -> GenTxId (BlockOf era) -> TxId
fromGenTxId EraShelley{} (Shelley.ShelleyTxId (ShelleyLedger.TxId i))  = TxId (Crypto.castHash i)
fromGenTxId EraByron{} (Byron.ByronTxId i) = fromByronTxId i
fromGenTxId EraByron{} _ = error "fromGenTxId:  unhandled Byron GenTxId case"

fromByronTxId :: Byron.TxId -> TxId
fromByronTxId =
  maybe (error "Failed to convert Byron txid.") TxId
  . Crypto.hashFromBytes . Byron.hashToBytes

fromByronTxIn :: Byron.TxIn -> TxIn
fromByronTxIn (Byron.TxInUtxo txid txix) =
  TxIn (fromByronTxId txid) (TxIx $ fromIntegral txix)

fromByronTxOut :: Byron.TxOut -> TxOut Byron
fromByronTxOut (Byron.TxOut addr coin) =
  TxOut (ByronAddress addr) (Lovelace $ Byron.lovelaceToInteger coin)

fromShelleyAddr :: Shelley.Addr TPraosStandardCrypto -> Address Shelley
fromShelleyAddr (Shelley.Addr nw pc scr) = ShelleyAddress nw pc scr
fromShelleyAddr _                        = error "fromShelleyAddr:  unhandled Shelley.Addr case"

fromShelleyLovelace :: Shelley.Coin -> Lovelace
fromShelleyLovelace (Shelley.Coin l) = Lovelace l

toByronTxId :: TxId -> Byron.TxId
toByronTxId (TxId h) =
  Byron.unsafeHashFromBytes (Crypto.hashToBytes h)

toByronTxIn  :: TxIn -> Byron.TxIn
toByronTxIn (TxIn txid (TxIx txix)) =
  Byron.TxInUtxo (toByronTxId txid) (fromIntegral txix)

toByronTxOut :: TxOut Byron -> Maybe Byron.TxOut
toByronTxOut (TxOut (ByronAddress addr) value) =
  Byron.TxOut addr <$> toByronLovelace value

toByronLovelace :: Lovelace -> Maybe Byron.Lovelace
toByronLovelace (Lovelace x) =
  case Byron.integerToLovelace x of
    Left  _  -> Nothing
    Right x' -> Just x'

signTransaction :: Era era -> SigningKeyOf era -> TxBody era -> Tx era
signTransaction p@EraByron{} k body =
  signByronTransaction (eraNetworkId p) body [k]
signTransaction EraShelley{} k body =
  signShelleyTransaction body [WitnessPaymentKey k]

mkTransaction :: forall era
  .  Era era
  -> SigningKeyOf era
  -> TxAdditionalSize
  -> TTL
  -> TxFee
  -> [TxIn]
  -> [TxOut era]
  -> Tx era
mkTransaction p key payloadSize ttl fee txins txouts =
  signTransaction p key $ makeTransaction p
 where
   makeTransaction :: Era era -> TxBody era
   makeTransaction EraShelley{} =
     makeShelleyTransaction
      (txExtraContentEmpty { txMetadata =
                             if payloadSize == 0
                             then Nothing
                             else Just $ payloadShelley payloadSize })
      ttl fee txins txouts
   makeTransaction EraByron{} =
     either (error . T.unpack) Prelude.id $
     mkByronTransaction txins txouts

   payloadShelley :: TxAdditionalSize -> TxMetadata
   payloadShelley = makeTransactionMetadata . Map.singleton 0 . TxMetaBytes . flip SB.replicate 42 . unTxAdditionalSize

   mkByronTransaction :: [TxIn]
                        -> [TxOut Byron]
                        -> Either Text (TxBody Byron)
   mkByronTransaction ins outs = do
     ins'  <- NonEmpty.nonEmpty ins        ?! error "makeByronTransaction: empty txIns"
     let ins'' = NonEmpty.map toByronTxIn ins'

     outs'  <- NonEmpty.nonEmpty outs      ?! error "makeByronTransaction: empty txOuts"
     outs'' <- traverse
                 (\out -> toByronTxOut out ?! error "makeByronTransaction: ByronTxBodyLovelaceOverflow")
                 outs'
     return $
       ByronTxBody $
         reAnnotate $
           Annotated
             (Byron.UnsafeTx ins'' outs'' (createTxAttributes payloadSize))
             ()
    where
     (?!) :: Maybe a -> e -> Either e a
     Nothing ?! e = Left e
     Just x  ?! _ = Right x

   -- | If this transaction should contain additional binary blob -
   --   we have to create attributes of the corresponding size.
   --   TxAttributes contains a map from 1-byte integer to arbitrary bytes which
   --   will be used as a binary blob to increase the size of the transaction.
   createTxAttributes
     :: TxAdditionalSize
     -> Byron.TxAttributes
   createTxAttributes (TxAdditionalSize 0) = Byron.mkAttributes ()
   createTxAttributes (TxAdditionalSize n) = blobAttributes n
    where
     blobAttributes :: Int -> Byron.TxAttributes
     blobAttributes aSize =
       (Byron.mkAttributes ()) {
         Byron.attrRemain =
           Byron.UnparsedFields $
           Map.singleton k $ LB.replicate (finalSize aSize) byte
       }

     k :: Word8
     k = 1 -- Arbitrary key.

     -- Fill an attribute by the same arbitrary byte in each element.
     byte :: Word8
     byte = 0

     sizeOfKey :: Int
     sizeOfKey = 1

     -- Please note that actual binary size of attributes will be a little bit
     -- bigger than the size defined by user (via CLI argument), because size of
     -- singleton 'Map k v' isn't equal to the size of ('k' + 'v').
     finalSize :: Int -> Int64
     finalSize userDefinedSize = fromIntegral (userDefinedSize - sizeOfKey)

mkTransactionGen
  :: r ~ Int
  => Era era
  -> SigningKeyOf era
  -> NonEmpty (TxIn, TxOut era)
  -- ^ Non-empty list of (TxIn, TxOut) that will be used as
  -- inputs and the key to spend the associated value
  -> Maybe (Address era)
  -- ^ The address to associate with the 'change',
  -- if different from that of the first argument
  -> Set (r, TxOut era)
  -- ^ Each recipient and their payment details
  -> TxAdditionalSize
  -- ^ Optional size of additional binary blob in transaction (as 'txAttributes')
  -> TxFee
  -- ^ Tx fee.
  -> ( Maybe (TxIx, Lovelace)   -- The 'change' index and value (if any)
     , Lovelace                 -- The associated fees
     , Map r TxIx               -- The offset map in the transaction below
     , Tx era
     )
mkTransactionGen p signingKey inputs mChangeAddr paySet payloadSize fee@(Lovelace fees) =
  (mChange, fee, offsetMap, tx)
 where
  tx = mkTransaction p signingKey payloadSize (SlotNo 10000000) fee
         (NonEmpty.toList $ fst <$> inputs)
         (NonEmpty.toList txOutputs)

  fundsTxOuts   = snd <$> NonEmpty.toList inputs
  payments      = toList paySet
  payTxOuts     = map snd payments

  Lovelace totalInpValue = txOutSum fundsTxOuts
  Lovelace totalOutValue = txOutSum payTxOuts
  changeValue@(Lovelace chgValRaw)
                = Lovelace (totalInpValue - totalOutValue - fees)

      -- change the order of comparisons first check emptiness of txouts AND remove appendr after

  (txOutputs, mChange) =
    if chgValRaw > 0
    then
      let changeAddress = fromMaybe (txOutAddr . snd $ NonEmpty.head inputs) mChangeAddr
          changeTxOut   = TxOut changeAddress changeValue
          changeIndex   = TxIx $ fromIntegral $ length payTxOuts -- 0-based index
      in
          (appendr payTxOuts (changeTxOut :| []), Just (changeIndex, changeValue))
    else
      case payTxOuts of
        []                 -> panic "change is zero and txouts is empty"
        txout0: txoutsRest -> (txout0 :| txoutsRest, Nothing)

  -- TxOuts of recipients are placed at the first positions
  offsetMap = Map.fromList $ zipWith (\payment index -> (fst payment, TxIx index))
                                     payments
                                     [0..]
  txOutSum :: Foldable t => t (TxOut era) -> Lovelace
  txOutSum =
    foldl' (\(Lovelace acc) (TxOut _ (Lovelace val)) -> Lovelace $ acc + val)
           (Lovelace 0)

  -- | Append a non-empty list to a list.
  -- > appendr [1,2,3] (4 :| [5]) == 1 :| [2,3,4,5]
  appendr :: [a] -> NonEmpty a -> NonEmpty a
  appendr l nel = foldr NonEmpty.cons nel l

  txOutAddr :: TxOut era -> Address era
  txOutAddr (TxOut addr _) = addr
