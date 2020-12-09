{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
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
  , mkTxOutValueAdaOnly
  , txOutValueLovelace
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
import qualified Ouroboros.Consensus.HardFork.Combinator.Embed.Unary as HFC
import           Ouroboros.Consensus.Ledger.SupportsMempool hiding (TxId)
import           Ouroboros.Consensus.TypeFamilyWrappers

-- Cardano-specific imports
import           Ouroboros.Consensus.Cardano.Block hiding (TxId, ShelleyEra)

-- Byron-specific imports
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Hashing as Byron
import qualified Ouroboros.Consensus.Byron.Ledger as Byron hiding (TxId)

-- Shelley-specific imports
import qualified Ouroboros.Consensus.Shelley.Ledger.Mempool as Shelley
import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.API as Shelley
import qualified Shelley.Spec.Ledger.TxBody as ShelleyLedger

import           Cardano.API
import           Cardano.Api.Shelley hiding (fromShelleyAddr)
import           Cardano.Api.TxSubmit
import           Cardano.Api.Typed
import           Cardano.Benchmarking.GeneratorTx.Era
import           Cardano.Benchmarking.GeneratorTx.Tx.Byron


-- https://github.com/input-output-hk/cardano-node/issues/1858 is the proper solution.
castTxMode :: Mode mode era -> Tx era -> TxForMode mode
castTxMode ModeByron{}          tx@ByronTx{}   = TxForByronMode  tx
castTxMode ModeShelley{}        tx@ShelleyTx{} = TxForShelleyMode tx
castTxMode ModeCardanoByron{}   tx@ByronTx{}   = TxForCardanoMode $ InAnyCardanoEra ByronEra tx
castTxMode ModeCardanoShelley{} tx@ShelleyTx{} = TxForCardanoMode $ InAnyCardanoEra ShelleyEra tx
castTxMode _ _ = error "castTxMode unsupported case"

-- https://github.com/input-output-hk/cardano-node/issues/1853 would be the long-term solution.
toGenTx :: Mode mode era -> Tx era -> GenTx (HFCBlockOf mode)
toGenTx ModeShelley{}        (ShelleyTx _ tx) = inject $ Shelley.mkShelleyTx tx
toGenTx ModeByron{}          (ByronTx tx)   = inject $ normalByronTxToGenTx tx
toGenTx ModeCardanoShelley{} (ShelleyTx _ tx) = GenTxShelley $ Shelley.mkShelleyTx tx
toGenTx ModeCardanoByron{}   (ByronTx tx)   = GenTxByron   $ normalByronTxToGenTx tx
toGenTx _ _ = error "toGenTx unsupported case"

shelleyTxId :: GenTxId (BlockOf ShelleyMode) -> TxId
shelleyTxId (Shelley.ShelleyTxId (ShelleyLedger.TxId i)) = TxId (Crypto.castHash i)

txOutAddress :: TxOut era -> AddressInEra era
txOutAddress = \case
  TxOut a@(AddressInEra ByronAddressInAnyEra{} _) _ -> a
  TxOut a@(AddressInEra ShelleyAddressInEra{}  _) _ -> a

mkTxOutValueAdaOnly :: Era era -> Lovelace -> TxOutValue era
mkTxOutValueAdaOnly = \case
  EraByron{}   -> TxOutAdaOnly AdaOnlyInByronEra
  EraShelley{} -> TxOutAdaOnly AdaOnlyInShelleyEra

txOutValueLovelace :: TxOutValue era -> Lovelace
txOutValueLovelace = \case
  TxOutAdaOnly AdaOnlyInByronEra   x -> x
  TxOutAdaOnly AdaOnlyInShelleyEra x -> x
  TxOutAdaOnly AdaOnlyInAllegraEra x -> x
  _ -> error "txOutValueLovelace unsupported case"
 
-- https://github.com/input-output-hk/cardano-node/issues/1859 is the proper solution.
fromGenTxId :: Mode mode era -> GenTxId (HFCBlockOf mode) -> TxId
fromGenTxId ModeShelley{}
  (HFC.project' (Proxy @(WrapGenTxId ShelleyBlock)) -> x) = shelleyTxId x
fromGenTxId ModeByron{}
  (HFC.project' (Proxy @(WrapGenTxId Byron.ByronBlock)) -> (Byron.ByronTxId i)) = fromByronTxId i
fromGenTxId ModeCardanoShelley{} (GenTxIdShelley x)  = shelleyTxId x
fromGenTxId ModeCardanoByron{}   (GenTxIdByron   (Byron.ByronTxId i))                           = fromByronTxId i
fromGenTxId _ _ =
  error "fromGenTxId:  unsupported protocol"

fromByronTxId :: Byron.TxId -> TxId
fromByronTxId =
  maybe (error "Failed to convert Byron txid.") TxId
  . Crypto.hashFromBytes . Byron.hashToBytes

fromByronTxIn :: Byron.TxIn -> TxIn
fromByronTxIn (Byron.TxInUtxo txid txix) =
  TxIn (fromByronTxId txid) (TxIx $ fromIntegral txix)

fromByronTxOut :: Byron.TxOut -> TxOut ByronEra
fromByronTxOut (Byron.TxOut addr coin) =
  TxOut (AddressInEra ByronAddressInAnyEra $ ByronAddress addr)
        (TxOutAdaOnly AdaOnlyInByronEra . Lovelace $ Byron.lovelaceToInteger coin)

fromShelleyAddr :: Mode mode ShelleyEra -> Shelley.Addr StandardShelley -> AddressInEra ShelleyEra
fromShelleyAddr m (Shelley.Addr _ pcr sref) =
  makeShelleyAddressInEra (modeNetworkId m) (fromShelleyPaymentCredential pcr) (fromShelleyStakeReference sref)
fromShelleyAddr _ _ = error "fromShelleyAddr:  unhandled Shelley.Addr case"

toByronTxId :: TxId -> Byron.TxId
toByronTxId (TxId h) =
  Byron.unsafeHashFromBytes (Crypto.hashToBytes h)

toByronTxIn  :: TxIn -> Byron.TxIn
toByronTxIn (TxIn txid (TxIx txix)) =
  Byron.TxInUtxo (toByronTxId txid) (fromIntegral txix)

toByronTxOut :: TxOut ByronEra -> Maybe Byron.TxOut
toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress addr)) (TxOutAdaOnly AdaOnlyInByronEra value)) =
  Byron.TxOut addr <$> toByronLovelace value
toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _addr)) (TxOutValue e _ )) = case e of {}
toByronTxOut (TxOut (AddressInEra (ShelleyAddressInEra e) _ ) _) = case e of {}

toByronLovelace :: Lovelace -> Maybe Byron.Lovelace
toByronLovelace (Lovelace x) =
  case Byron.integerToLovelace x of
    Left  _  -> Nothing
    Right x' -> Just x'

signTransaction :: Mode mode era -> SigningKeyOf era -> TxBody era -> Tx era
signTransaction p k body = case modeEra p of
  EraByron   -> signByronTransaction (modeNetworkId p) body [k]
  EraShelley -> signShelleyTransaction body [WitnessPaymentKey k]


mkTransaction :: forall mode era
  .  Mode mode era
  -> SigningKeyOf era
  -> TxAdditionalSize
  -> SlotNo
  -> Lovelace
  -> [TxIn]
  -> [TxOut era]
  -> Tx era
mkTransaction p key payloadSize ttl fee txins txouts =
  signTransaction p key $ makeTransaction p
  -- https://github.com/input-output-hk/cardano-node/issues/1854 would be the long-term solution.
 where
   makeTransaction :: Mode mode era -> TxBody era
   makeTransaction m = case modeEra m of
     EraShelley -> case makeShelleyTransaction txins txouts ttl fee [] [] metaData Nothing of
                      Right tx -> tx
                      Left err -> error $ show err
                   where metaData = if payloadSize == 0 then Nothing else Just $ payloadShelley payloadSize
     EraByron ->
       either (error . T.unpack) Prelude.id $
         mkByronTransaction txins txouts

   payloadShelley :: TxAdditionalSize -> TxMetadata
   payloadShelley = makeTransactionMetadata . Map.singleton 0 . TxMetaBytes . flip SB.replicate 42 . unTxAdditionalSize

   mkByronTransaction :: [TxIn]
                        -> [TxOut ByronEra]
                        -> Either Text (TxBody ByronEra)
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
  :: (r ~ Int)
  => Mode mode era
  -> SigningKeyOf era
  -> NonEmpty (TxIn, TxOut era)
  -- ^ Non-empty list of (TxIn, TxOut) that will be used as
  -- inputs and the key to spend the associated value
  -> Maybe (AddressInEra era)
  -- ^ The address to associate with the 'change',
  -- if different from that of the first argument
  -> [(r, TxOut era)]
  -- ^ Each recipient and their payment details
  -> TxAdditionalSize
  -- ^ Optional size of additional binary blob in transaction (as 'txAttributes')
  -> Lovelace
  -- ^ Tx fee.
  -> ( Maybe (TxIx, Lovelace)   -- The 'change' index and value (if any)
     , Lovelace                 -- The associated fees
     , Map r TxIx               -- The offset map in the transaction below
     , Tx era
     )
mkTransactionGen p signingKey inputs mChangeAddr payments payloadSize fee@(Lovelace fees) =
  (mChange, fee, offsetMap, tx)
 where
  tx = mkTransaction p signingKey payloadSize (SlotNo 10000000)
         fee
         (NonEmpty.toList $ fst <$> inputs)
         (NonEmpty.toList txOutputs)

  fundsTxOuts   = snd <$> NonEmpty.toList inputs
  payTxOuts     = map snd payments

  Lovelace totalInpValue = txOutSum fundsTxOuts
  Lovelace totalOutValue = txOutSum payTxOuts
  changeValue@(Lovelace chgValRaw)
                = Lovelace (totalInpValue - totalOutValue - fees)

      -- change the order of comparisons first check emptiness of txouts AND remove appendr after

  (txOutputs, mChange) =
    if chgValRaw > 0
    then
      let changeAddress = fromMaybe (txOutAddress . snd $ NonEmpty.head inputs) mChangeAddr
          changeTxOut   = TxOut changeAddress $ mkTxOutValueAdaOnly (modeEra p) changeValue
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
  txOutSum :: [(TxOut era)] -> Lovelace
  txOutSum l = sum $ map toVal l
  toVal :: forall era. TxOut era -> Lovelace
  toVal (TxOut _ (TxOutAdaOnly _ val)) = val
  toVal (TxOut (AddressInEra _ _) (TxOutValue _ _)) = error "unexpected multiAsset"

  -- | Append a non-empty list to a list.
  -- > appendr [1,2,3] (4 :| [5]) == 1 :| [2,3,4,5]
  appendr :: [a] -> NonEmpty a -> NonEmpty a
  appendr l nel = foldr NonEmpty.cons nel l
