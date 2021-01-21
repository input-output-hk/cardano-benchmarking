{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Cardano.Benchmarking.GeneratorTx.SizedMetadata
where

import           Prelude

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import           Cardano.Benchmarking.GeneratorTx.Tx
import           Cardano.Api

maxMapSize :: Int
maxMapSize = 1000
maxBSSize :: Int
maxBSSize = 64

-- Properties of the underlying/opaque CBOR encoding.
assume_cbor_properties :: Bool
assume_cbor_properties
  =    prop_mapCostsShelley
    && prop_mapCostsAllegra
    && prop_mapCostsMary
    && prop_bsCostsSelley
    && prop_bsCostsAllegra
    && prop_bsCostsMary

-- The cost of map entries in metadata follows a step function.
-- This assums the map indecies are [0..n].
prop_mapCostsShelley :: Bool
prop_mapCostsAllegra :: Bool
prop_mapCostsMary    :: Bool
prop_mapCostsShelley = measureMapCosts AsShelleyEra == assumeMapCosts AsShelleyEra
prop_mapCostsAllegra = measureMapCosts AsAllegraEra == assumeMapCosts AsAllegraEra
prop_mapCostsMary    = measureMapCosts AsMaryEra    == assumeMapCosts AsMaryEra

assumeMapCosts :: forall era . IsShelleyBasedEra era => AsType era -> [Int]
assumeMapCosts _proxy = stepFunction [
      (   1 , 0)          -- An empty map of metadata has the same cost as TxMetadataNone.
    , (   1 , firstEntry) -- Using Metadata costs 37 or 39 bytes  (first map entry).
    , (  22 , 2)          -- The next 22 entries cost 2 bytes each.
    , ( 233 , 3)          -- 233 entries at 3 bytes.
    , ( 744 , 4)          -- 744 entries at 4 bytes.
    ]
  where
    firstEntry = case shelleyBasedEra @ era of
      ShelleyBasedEraShelley -> 37
      ShelleyBasedEraAllegra -> 39
      ShelleyBasedEraMary    -> 39

-- Bytestring costs are not LINEAR !!
-- Costs are piecewise linear for payload sizes [0..24] and [25..64].
prop_bsCostsSelley  :: Bool
prop_bsCostsAllegra :: Bool
prop_bsCostsMary    :: Bool
prop_bsCostsSelley  = measureBSCosts AsShelleyEra == [37..60] ++ [62..102]
prop_bsCostsAllegra = measureBSCosts AsAllegraEra == [39..62] ++ [64..104]
prop_bsCostsMary    = measureBSCosts AsMaryEra    == [39..62] ++ [64..104]

stepFunction :: [(Int, Int)] -> [Int]
stepFunction f = scanl1 (+) steps
  where
    steps = concatMap (\(count,step) -> replicate count step) f

-- Measure the cost of metadata map entries.
-- This is the cost of the index with an empty BS as payload.
measureMapCosts :: forall era . IsShelleyBasedEra era => AsType era -> [Int]
measureMapCosts era = map (metadataSize era . Just . replicateEmptyBS) [0..maxMapSize]
  where
    replicateEmptyBS :: Int -> TxMetadata
    replicateEmptyBS n = listMetadata $ replicate n $ TxMetaBytes $ BS.empty

listMetadata :: [TxMetadataValue] -> TxMetadata
listMetadata l = makeTransactionMetadata $ Map.fromList $ zip [0..] l

-- Cost of metadata with a single BS of size [0..maxBSSize].
measureBSCosts :: forall era . IsShelleyBasedEra era => AsType era -> [Int]
measureBSCosts era = map (metadataSize era . Just . bsMetadata) [0..maxBSSize]
  where bsMetadata s = listMetadata [TxMetaBytes $ BS.replicate s 0]

metadataSize :: forall era . IsShelleyBasedEra era => AsType era -> Maybe TxMetadata -> Int
metadataSize p m = dummyTxSize p m - dummyTxSize p Nothing

dummyTxSize :: forall era . IsShelleyBasedEra era => AsType era -> Maybe TxMetadata -> Int
dummyTxSize _p metadata = case makeTransactionBody dummyTx of
    Right b -> BS.length $ serialiseToCBOR b
    Left err -> error $ "metaDataSize " ++ show err
  where
    dataInEra :: Maybe TxMetadata -> TxMetadataInEra era
    dataInEra Nothing = TxMetadataNone
    dataInEra (Just m) = case shelleyBasedEra @ era of
      ShelleyBasedEraShelley -> TxMetadataInEra TxMetadataInShelleyEra m
      ShelleyBasedEraAllegra -> TxMetadataInEra TxMetadataInAllegraEra m
      ShelleyBasedEraMary    -> TxMetadataInEra TxMetadataInMaryEra m

    dummyTx = TxBodyContent {
        txIns = [ TxIn "dbaff4e270cfb55612d9e2ac4658a27c79da4a5271c6f90853042d1403733810" (TxIx 0) ]
      , txOuts = []
      , txFee = mkFee $ fromInteger 0
      , txValidityRange = (TxValidityNoLowerBound, mkValidityUpperBound $ fromInteger 0)
      , txMetadata = dataInEra metadata
      , txAuxScripts = TxAuxScriptsNone
      , txWithdrawals = TxWithdrawalsNone
      , txCertificates = TxCertificatesNone
      , txUpdateProposal = TxUpdateProposalNone
      , txMintValue = TxMintNone
      }
