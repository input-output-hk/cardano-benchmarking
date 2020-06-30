{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Benchmarking.GeneratorTx
  ( NumberOfTxs(..)
  , NumberOfInputsPerTx(..)
  , NumberOfOutputsPerTx(..)
  , InitCooldown(..)
  , FeePerTx(..)
  , TPSRate(..)
  , TxAdditionalSize(..)
  , TxGenError
  , genesisBenchmarkRunner
  ) where

import           Cardano.Prelude
import           Prelude (String, error, fail)

import           Codec.CBOR.Read (deserialiseFromBytes)
import           Control.Concurrent (threadDelay)
import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Control.Monad (forM, forM_, mapM, when)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left, right, firstExceptT)
import           Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as LB
import           Data.Either (isLeft)
import           Data.Foldable (find, foldl', foldr, toList)
import qualified Data.IP as IP
import           Data.List ((!!))
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (Maybe (..), fromMaybe, mapMaybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word8, Word32, Word64)
import           Network.Socket (AddrInfo (..),
                     AddrInfoFlag (..), Family (..), SocketType (Stream),
                     addrFamily,addrFlags, addrSocketType, defaultHints,
                     getAddrInfo)

import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.UTxO as CC.UTxO
import           Cardano.Config.Logging (LoggingLayer (..))
import           Cardano.Config.Types (NodeAddress (..), NodeHostAddress(..),
                                       SocketPath)
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Hash.Class   as Crypto

import           Cardano.Api
import qualified Cardano.Api.Shelley.ColdKeys as ColdKeys
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Mempool as Shelley hiding (TxId(..))
import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Genesis as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.TxData as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

import           Cardano.Benchmarking.GeneratorTx.Error (TxGenError (..))
import           Cardano.Benchmarking.GeneratorTx.NodeToNode (BenchmarkTxSubmitTracers (..),
                     benchmarkConnectTxSubmit)
import           Cardano.Benchmarking.GeneratorTx.Submission
import           Cardano.Benchmarking.GeneratorTx.Tx (txSpendGenesisUTxOByronPBFT,
                     normalByronTxToGenTx)

import           Control.Tracer (Tracer, traceWith)

import           Ouroboros.Network.NodeToClient (IOManager)

import           Ouroboros.Consensus.Node.Run (RunNode)
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Ouroboros.Consensus.Config (TopLevelConfig(..))
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..),
                                                   GenTx (..),
                                                   byronProtocolMagicId)
import           Ouroboros.Consensus.Ledger.SupportsMempool as Mempool
                   ( GenTxId )

newtype NumberOfTxs =
  NumberOfTxs Word64
  deriving (Eq, Ord, Show)

newtype NumberOfInputsPerTx =
  NumberOfInputsPerTx Int
  deriving (Eq, Ord, Show)

newtype NumberOfOutputsPerTx =
  NumberOfOutputsPerTx Int
  deriving (Eq, Ord, Show)

newtype FeePerTx =
  FeePerTx Word64
  deriving (Eq, Ord, Show)

-- | How long wait before starting the main submission phase,
--   after the init Tx batch was submitted.
newtype InitCooldown =
  InitCooldown Int
  deriving (Eq, Ord, Show)

-- | This parameter specifies additional size (in bytes) of transaction.
--   Since 1 transaction is ([input] + [output] + attributes), its size
--   is defined by its inputs and outputs. We want to have an ability to
--   increase transaction's size without increasing the number of inputs/
--   outputs. Such a big transaction will give us more real-world results
--   of benchmarking.
--   Technically this parameter specifies the size of attribute we'll
--   add to transaction (by default attributes are empty, so if this
--   parameter is skipped, attributes will remain empty).
newtype TxAdditionalSize =
  TxAdditionalSize Int
  deriving (Eq, Ord, Show)

-- TODO: move out to Types.hs
paramsNetwork :: Params ShelleyBlock -> Network
paramsNetwork (ParamsShelley
                TopLevelConfig
                  { configLedger = Shelley.ShelleyLedgerConfig
                    { Shelley.shelleyLedgerGenesis = Consensus.ShelleyGenesis
                      { Consensus.sgNetworkMagic } }}
                _)
  = Testnet $ NetworkMagic sgNetworkMagic

-----------------------------------------------------------------------------------------
-- | Genesis benchmark runner (we call it in 'Run.runNode').
--
--   Using a _richman_ (from genesis block) to supply some initial
--   amount of funds for disbursment.
-----------------------------------------------------------------------------------------
genesisBenchmarkRunner
  :: forall blk p
  .  ( BenchTraceConstraints blk
     )
  => LoggingLayer
  -> IOManager
  -> SocketPath
  -> Consensus.Protocol IO blk p
  -> NonEmpty NodeAddress
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> NumberOfOutputsPerTx
  -> FeePerTx
  -> TPSRate
  -> InitCooldown
  -> Maybe TxAdditionalSize
  -> [FilePath]
  -> Bool
  -> ExceptT TxGenError IO ()
genesisBenchmarkRunner loggingLayer
                       iocp
                       socketFp
                       protocol@Consensus.ProtocolRealPBFT{}
                       targetNodeAddresses
                       numOfTxs@(NumberOfTxs rawNumOfTxs)
                       numOfInsPerTx
                       numOfOutsPerTx
                       txFee
                       tpsRate@(TPSRate tps)
                       initCooldown
                       txAdditionalSize
                       signingKeyFiles
                       singleThreaded = do
  when (length signingKeyFiles < 3) $
    left $ NeedMinimumThreeSigningKeyFiles signingKeyFiles

  -- Since currently slot is 20 sec, we want to check lower limit 1/20 (minimum one tx per block).
  when (tps < 0.05) $
    left $ TooSmallTPSRate tps

  let p = mkParamsByron protocol loggingLayer

  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator, tracers are ready *******"

  -- 'genesisKey' is for genesis address with initial amount of money (1.4 billion ADA for now).
  -- 'sourceKey' is for source address that we'll use as a source of money for next transactions.
  -- 'recepientKey' is for recipient address that we'll use as an output for next transactions.
  (genesisKey:sourceKey:recepientKey:_) <- prepareSigningKeys p signingKeyFiles

  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator, signing keys are ready *******"

  -- We have to prepare an initial funds (it's the money we'll send from 'genesisKey' to
  -- 'sourceKey'), this will be our very first transaction.
  fundsWithGenesisMoney <- liftIO $
    prepareInitialFundsByron
                        p
                        iocp
                        socketFp
                        (extractGenesisFunds p [genesisKey])
                        (mkAddressForKey p genesisKey)
                        (mkAddressForKey p sourceKey)
                        txFee

  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator, initial funds are prepared (sent to sourceAddress) *******"

  -- Check if no transactions needed...
  when (rawNumOfTxs > 0) $
    runBenchmark p
                 iocp
                 socketFp
                 sourceKey
                 (mkAddressForKey p recepientKey)
                 (if singleThreaded
                   then NE.fromList $ NE.take 1 targetNodeAddresses
                   else targetNodeAddresses)
                 numOfTxs
                 numOfInsPerTx
                 numOfOutsPerTx
                 txFee
                 tpsRate
                 initCooldown
                 txAdditionalSize
                 fundsWithGenesisMoney

genesisBenchmarkRunner loggingLayer
                       iocp
                       socketFp
                       protocol@Consensus.ProtocolRealTPraos{}
                       targetNodeAddresses
                       numOfTxs@NumberOfTxs{}
                       numOfInsPerTx
                       numOfOutsPerTx
                       txFee
                       tpsRate@(TPSRate tps)
                       initCooldown
                       txAdditionalSize
                       signingKeyFiles
                       singleThreaded = do
  when (length signingKeyFiles < 3) $
    left $ NeedMinimumThreeSigningKeyFiles signingKeyFiles

  -- Since currently slot is 20 sec, we want to check lower limit 1/20 (minimum one tx per block).
  when (tps < 0.05) $
    left $ TooSmallTPSRate tps

  let p = mkParamsShelley protocol loggingLayer

  (genesisKey:sourceKey:recepientKey:_) <- prepareSigningKeys p signingKeyFiles
  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator, signing keys are ready *******"

  genesisFunds :: (TxDetailsShelley, SigningKey)
    <- pure $! extractGenesisFunds p [genesisKey]

  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator, genesis funds ready: " <> show genesisFunds

  fundsWithGenesisMoney <- liftIO $
    prepareInitialFundsShelley
                        p
                        socketFp
                        genesisFunds
                        (mkAddressForKey p genesisKey)
                        (mkAddressForKey p sourceKey)
                        txFee

  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator, initial funds are prepared (sent to sourceAddress) *******"

  runBenchmark p
               iocp
               socketFp
               sourceKey
               (mkAddressForKey p recepientKey)
               (if singleThreaded
                 then NE.fromList $ NE.take 1 targetNodeAddresses
                 else targetNodeAddresses)
               numOfTxs
               numOfInsPerTx
               numOfOutsPerTx
               txFee
               tpsRate
               initCooldown
               txAdditionalSize
               fundsWithGenesisMoney

{-------------------------------------------------------------------------------
  Main logic
-------------------------------------------------------------------------------}

-- | The store of the unspent funds (available transaction outputs)
type TxDetailsByron   = TxDetails ByronBlock
type TxDetailsShelley = TxDetails ShelleyBlock

data TxDetails blk where
  TxDetailsByron
    :: !(CC.UTxO.TxIn, CC.UTxO.TxOut)
    -> TxDetails ByronBlock
  TxDetailsShelley
    :: !(TxIn, TxOut)
    -> TxDetails ShelleyBlock

deriving instance (Ord  TxIn)
deriving instance (Ord  TxOut)
deriving instance (Ord  TxId)
deriving instance (Ord  Lovelace)

deriving instance (Eq   (TxDetails blk))
deriving instance (Ord  (TxDetails blk))
deriving instance (Show (TxDetails blk))

fromByronLovelace :: CC.Common.Lovelace -> Lovelace
fromByronLovelace = Lovelace . fromIntegral . CC.Common.unsafeGetLovelace

txdTxOutAddr :: TxDetails blk -> Address
txdTxOutAddr (txdTxOut -> TxOut a _) = a

txdTxOutValue :: TxDetails blk -> Lovelace
txdTxOutValue (txdTxOut -> TxOut _ v) = v

txdTxOut :: TxDetails blk -> TxOut
txdTxOut (TxDetailsByron (_, CC.UTxO.TxOut addr val)) =
  TxOut (AddressByron addr) (fromByronLovelace val)
txdTxOut (TxDetailsShelley (_, txout)) = txout

toByronLovelace :: Lovelace -> CC.Common.Lovelace
toByronLovelace (Lovelace x) = x' where Right x' = CC.Common.integerToLovelace x

fromShelleyTxIn :: Shelley.TxIn crypto -> TxIn
fromShelleyTxIn (Shelley.TxIn txid ix) = TxIn (fromShelleyTxId txid) (fromIntegral ix)

fromShelleyTxId :: Shelley.TxId crypto -> TxId
fromShelleyTxId (Shelley.TxId (Crypto.UnsafeHash h)) = TxId (Crypto.UnsafeHash h)

-- TBC
class (Ord r) => FiscalRecipient r where

instance FiscalRecipient Int where

-----------------------------------------------------------------------------------------
-- | Prepare signing keys and addresses for transactions.
-----------------------------------------------------------------------------------------

-- | We take files with secret keys and create signing keys from them,
--   we need it to be able to spend money from corresponding addresses.
prepareSigningKeys :: Params blk -> [FilePath] -> ExceptT TxGenError IO [SigningKey]
prepareSigningKeys ParamsByron{} skeys = do
  bsList <- liftIO $ mapM readSecretKey skeys
  when (any isLeft bsList) $
    left . SecretKeyReadError . show $ filter isLeft bsList

  let desKeys = map (deserialiseFromBytes Crypto.fromCBORXPrv) $ rights bsList

  when (any isLeft desKeys) $
    left . SecretKeyDeserialiseError . show . (fmap . fmap) fst $ filter isLeft desKeys
  pure . map (SigningKeyByron . Crypto.SigningKey . snd) $ rights desKeys
 where
   readSecretKey :: FilePath -> IO (Either String LB.ByteString)
   readSecretKey skFp = do
     eBs <- Exception.try $ LB.readFile skFp
     case eBs of
       Left e -> pure . Left $ handler e
       Right lbs -> pure $ Right lbs
    where
      handler :: IOException -> String
      handler e = "Cardano.CLI.Benchmarking.Tx.Generation.readSecretKey: "
                  ++ displayException e

prepareSigningKeys ParamsShelley{} skeys =
  firstExceptT (SecretKeyReadError . show) $
    mapM (fmap SigningKeyShelley <$>
          ColdKeys.readSigningKey ColdKeys.GenesisUTxOKey) skeys

mkAddressForKey :: Params blk -> SigningKey -> Address
mkAddressForKey (ParamsByron
                  TopLevelConfig
                   { configLedger = genesisConfig } _)
                (SigningKeyByron key) =
  AddressByron . CC.Common.makeVerKeyAddress networkMagic . Crypto.toVerification $ key
 where
  networkMagic = CC.Common.makeNetworkMagic (CC.Genesis.configProtocolMagic genesisConfig)
mkAddressForKey p@ParamsShelley{}
                (SigningKeyShelley key) =
  shelleyVerificationKeyAddress
    (paramsNetwork p)
    (PaymentVerificationKeyShelley . Shelley.VKey $ Crypto.deriveVerKeyDSIGN key)
    Nothing
mkAddressForKey _ _ = error "Byron/Shelley-only"

-----------------------------------------------------------------------------------------
-- Extract access to the Genesis funds.
-----------------------------------------------------------------------------------------

-- Extract the UTxO in the genesis block for the rich men
extractGenesisFunds
  :: Params blk
  -> [SigningKey]
  -> (TxDetails blk, SigningKey)

extractGenesisFunds (ParamsByron
                     TopLevelConfig
                      { configLedger = genesisConfig
                      } _) signingKeys =
    (Map.! 0)
  . Map.fromList
  . mapMaybe (\txd@(TxDetailsByron (_inp, out)) ->
                mkEntry txd <$> isRichman out)
  . fromCompactTxInTxOutList
  . Map.toList
  . CC.UTxO.unUTxO
  . CC.UTxO.genesisUtxo
  $ genesisConfig
 where
  mkEntry
    :: TxDetailsByron
    -> (Int, SigningKey)
    -> (Int, (TxDetailsByron, SigningKey))
  mkEntry txd (richman, key) = (richman, (txd, key))

  isRichman :: CC.UTxO.TxOut -> Maybe (Int, SigningKey)
  isRichman out = find (isValidKey . snd) richmen
    where
      isValidKey :: SigningKey -> Bool
      isValidKey (SigningKeyByron key) =
        CC.Common.checkVerKeyAddress (Crypto.toVerification key) (CC.UTxO.txOutAddress out)

  richmen :: [(Int, SigningKey)]
  richmen = zip [0..] signingKeys

  fromCompactTxInTxOutList
    :: [(CC.UTxO.CompactTxIn, CC.UTxO.CompactTxOut)]
    -> [TxDetailsByron]
  fromCompactTxInTxOutList =
    map (TxDetailsByron . bimap CC.UTxO.fromCompactTxIn CC.UTxO.fromCompactTxOut)

extractGenesisFunds p@(ParamsShelley
                       TopLevelConfig
                        { configLedger = Shelley.ShelleyLedgerConfig
                          { Shelley.shelleyLedgerGenesis = genesis }
                        } _) signingKeys =
    (Map.! 0)
  . Map.fromList
  . mapMaybe (\txd@(TxDetailsShelley (_inp, out)) ->
                mkEntry txd <$> isRichman out)
  . fmap genesisFundsEntryTxDetails
  . Map.toList
  . Consensus.sgInitialFunds
  $ genesis
 where
  genesisFundsEntryTxDetails
    :: (Shelley.Addr TPraosStandardCrypto, Shelley.Coin) -> TxDetailsShelley
  genesisFundsEntryTxDetails (addr, Shelley.Coin coin) =
    TxDetailsShelley ( fromShelleyTxIn genesisTxIn
                     , TxOut (AddressShelley addr) (Lovelace coin))
   where genesisTxIn = Shelley.initialFundsPseudoTxIn addr

  mkEntry
    :: TxDetailsShelley
    -> (Int, SigningKey)
    -> (Int, (TxDetailsShelley, SigningKey))
  mkEntry txd (richman, key) = (richman, (txd, key))

  isRichman :: TxOut -> Maybe (Int, SigningKey)
  isRichman (TxOut addr _) =
    find (isKeyForTxOut . snd) richmen
   where
      isKeyForTxOut :: SigningKey -> Bool
      isKeyForTxOut key = mkAddressForKey p key == addr

  richmen :: [(Int, SigningKey)]
  richmen = zip [0..] signingKeys

-----------------------------------------------------------------------------------------
-- Work with initial funds.
-----------------------------------------------------------------------------------------

-- Prepare and submit our first transaction: send money from 'initialAddress' to 'sourceAddress'
-- (latter corresponds to 'targetAddress' here) and "remember" it in 'availableFunds'.
prepareInitialFundsByron
  :: (blk ~ ByronBlock)
  => Params blk
  -> IOManager
  -> SocketPath
  -> (TxDetails blk, SigningKey)
  -> Address
  -> Address
  -> FeePerTx
  -> IO (Set (TxDetails blk))
prepareInitialFundsByron
  p@(ParamsByron
      TopLevelConfig
        { configLedger = genesisConfig } _)
  iocp socketFp
  (TxDetailsByron (_, genesisTxOut), SigningKeyByron signingKey)
  (AddressByron genesisAddress)
  (AddressByron targetAddress)
  (FeePerTx txFee) = do

    submitTx' p iocp socketFp genesisTxGeneral

    -- Form availableFunds with a single value, it will be used for further (splitting) transactions.
    return $ Set.singleton (TxDetailsByron (txDetIn, txDetOut))

  where
   txDetIn  = CC.UTxO.TxInUtxo (getTxIdFromGenTxByron genesisTxGeneral) 0

   genesisTxGeneral :: GenTx ByronBlock
   genesisTxGeneral = normalByronTxToGenTx genesisTx

   genesisTx :: CC.UTxO.ATxAux ByteString
   genesisTx = txSpendGenesisUTxOByronPBFT genesisConfig
                                           signingKey
                                           genesisAddress
                                           (NE.fromList [txDetOut])

   txDetOut = CC.UTxO.TxOut
     { CC.UTxO.txOutAddress = targetAddress
     , CC.UTxO.txOutValue   = CC.UTxO.txOutValue genesisTxOut `subLovelace` fee
     }

   fee :: CC.Common.Lovelace
   fee = assumeBound . CC.Common.mkLovelace $ txFee

getTxIdFromGenTxByron :: GenTx ByronBlock -> CC.UTxO.TxId
getTxIdFromGenTxByron (ByronTx txId _) = txId
getTxIdFromGenTxByron _ = panic "Impossible happened: generated transaction is not a ByronTx!"

getTxIdFromGenTxShelley :: GenTx ShelleyBlock -> TxId
getTxIdFromGenTxShelley (Shelley.ShelleyTx txId _) = fromShelleyTxId txId

toByronTxOut :: TxOut -> CC.UTxO.TxOut
toByronTxOut (TxOut (AddressByron addr) value) =
  CC.UTxO.TxOut addr (toByronLovelace value)

prepareInitialFundsShelley
  :: (blk ~ ShelleyBlock)
  => Params blk
  -> SocketPath
  -> (TxDetails blk, SigningKey)
  -> Address
  -> Address
  -> FeePerTx
  -> IO (Set (TxDetails blk))
prepareInitialFundsShelley
  p@ParamsShelley{}
  socketFp
  (TxDetailsShelley (_, TxOut (AddressShelley addr) genesisCoin), signingKey)
  _genesisAddress targetAddress (FeePerTx txFee) = do

    r <- submitTx network socketFp genesisTx

    liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
      $ "******* Genesis funds move submission result: " <> show r

    pure $ Set.singleton (TxDetailsShelley (txDetIn, txDetOut))

 where
   txDetIn  = fromShelleyTxIn $
                Shelley.TxIn (Shelley.TxId (Shelley.hashTxBody body)) 0

   genesisTx :: TxSigned
   genesisTx =
     signTransaction
       genesisTxUnsigned
       network
       [signingKey]

   genesisTxUnsigned :: TxUnsigned
   genesisTxUnsigned@(TxUnsignedShelley body) =
     buildShelleyTransaction
       [fromShelleyTxIn txInGenesis]
       [txDetOut]
       (SlotNo 10000000)
       (Lovelace . fromIntegral $ txFee)
       []
       (WithdrawalsShelley $ Shelley.Wdrl mempty)
       Nothing
       Nothing

   txDetOut = TxOut
                targetAddress
                (genesisCoin `subLovelace'` Lovelace (fromIntegral txFee))

   txInGenesis = Shelley.initialFundsPseudoTxIn addr

   network :: Network
   network = paramsNetwork p

-- | One or more inputs -> one or more outputs.
mkTransaction
  :: (FiscalRecipient r)
  => Params blk
  -> NonEmpty (TxDetails blk, SigningKey)
  -- ^ Non-empty list of (TxIn, TxOut) that will be used as
  -- inputs and the key to spend the associated value
  -> Maybe Address
  -- ^ The address to associate with the 'change',
  -- if different from that of the first argument
  -> Set (r, TxOut)
  -- ^ Each recipient and their payment details
  -> Maybe TxAdditionalSize
  -- ^ Optional size of additional binary blob in transaction (as 'txAttributes')
  -> Word64
  -- ^ Tx fee.
  -> ( Maybe (Word32, Lovelace) -- The 'change' index and value (if any)
     , Lovelace                 -- The associated fees
     , Map r Word32             -- The offset map in the transaction below
     , GenTx blk
     )
mkTransaction p@ParamsByron{}
              inputs mChangeAddress payments txAdditionalSize txFee =
  (mChange, fees, offsetMap, normalByronTxToGenTx txAux)
 where
  -- Each input contains the same 'signingKey' and the same 'txOutAddress',
  -- so pick the first one.
  (TxDetailsByron (_, firstTxOutFrom), signingKey) = NE.head inputs
  -- Take all txoutFrom's.
  allTxOutFrom  = NE.map (txdTxOut . fst) inputs

  paymentsList  = toList payments
  txOuts        = map snd paymentsList

  totalInpValue = foldl' (\s (TxOut _ val) -> s `addLovelace'` val)
                         (Lovelace 0)
                         allTxOutFrom

  totalOutValue = foldl' (\s (TxOut _ val) -> s `addLovelace'` val)
                         (Lovelace 0)
                         txOuts
  fees          = Lovelace $ fromIntegral txFee
  changeValue@(Lovelace chgValRaw)
                = totalInpValue `subLovelace'` (totalOutValue `addLovelace'` fees)

  -- change the order of comparisons first check emptiness of txouts AND remove appendr after
  (txOutputs, mChange) =
    if chgValRaw > 0
    then
      let changeAddress =
            fromMaybe (AddressByron $ CC.UTxO.txOutAddress firstTxOutFrom) mChangeAddress
          changeTxOut   = TxOut changeAddress changeValue
          changeIndex   = fromIntegral $ length txOuts -- 0-based index
      in
          (appendr txOuts (changeTxOut :| []), Just (changeIndex, changeValue))
    else
      case txOuts of
        []                 -> panic "change is zero and txouts is empty"
        txout0: txoutsRest -> (txout0 :| txoutsRest, Nothing)

  -- TxOuts of recipients are placed at the first positions
  offsetMap = Map.fromList $ zipWith (\payment index -> (fst payment, index))
                                     paymentsList
                                     [0..]

  -- Take all actual inputs.
  pureInputs = NE.map (\(TxDetailsByron (txIn, _), _) -> txIn) inputs

  tx :: CC.UTxO.Tx
  tx = CC.UTxO.UnsafeTx
         { CC.UTxO.txInputs     = pureInputs
         , CC.UTxO.txOutputs    = toByronTxOut <$> txOutputs
         , CC.UTxO.txAttributes = createTxAttributes txAdditionalSize
         }

  txAux = createTxAux (paramsConfig p) tx signingKey

mkTransaction p@ParamsShelley{}
              inputs mChangeAddress payments _txAdditionalSize txFee =
  (mChange, fees, offsetMap, Shelley.mkShelleyTx tx)
 where
  TxSignedShelley tx
    = signTransaction
        (buildShelleyTransaction
          (NE.toList pureInputs) (NE.toList txOutputs)
          (SlotNo 10000000)
          fees
          []
          (WithdrawalsShelley $ Shelley.Wdrl mempty)
          Nothing
          Nothing)
        (paramsNetwork p) [signingKey]

  pureInputs = NE.map (\(TxDetailsShelley (txIn, _), _) -> txIn) inputs

  -- Each input contains the same 'signingKey' and the same 'txOutAddress',
  -- so pick the first one.
  (txd, signingKey) = NE.head inputs
  -- Take all txoutFrom's.
  allTxOutFrom  = NE.map (txdTxOut . fst) inputs

  paymentsList  = toList payments
  txOuts        = map snd paymentsList

  totalInpValue = foldl' (\s (TxOut _ val) -> s `addLovelace'` val)
                         (Lovelace 0)
                         allTxOutFrom

  totalOutValue = foldl' (\s (TxOut _ val) -> s `addLovelace'` val)
                         (Lovelace 0)
                         txOuts
  fees          = Lovelace $ fromIntegral txFee
  changeValue@(Lovelace chgValRaw)
                = totalInpValue `subLovelace'` (totalOutValue `addLovelace'` fees)

      -- change the order of comparisons first check emptiness of txouts AND remove appendr after

  (txOutputs, mChange) =
    if chgValRaw > 0
    then
      let changeAddress =
            fromMaybe (txdTxOutAddr txd) mChangeAddress
          changeTxOut   = TxOut changeAddress changeValue
          changeIndex   = fromIntegral $ length txOuts -- 0-based index
      in
          (appendr txOuts (changeTxOut :| []), Just (changeIndex, changeValue))
    else
      case txOuts of
        []                 -> panic "change is zero and txouts is empty"
        txout0: txoutsRest -> (txout0 :| txoutsRest, Nothing)

  -- TxOuts of recipients are placed at the first positions
  offsetMap = Map.fromList $ zipWith (\payment index -> (fst payment, index))
                                     paymentsList
                                     [0..]

-- | If this transaction should contain additional binary blob -
--   we have to create attributes of the corresponding size.
--   TxAttributes contains a map from 1-byte integer to arbitrary bytes which
--   will be used as a binary blob to increase the size of the transaction.
createTxAttributes
  :: Maybe TxAdditionalSize
  -> CC.UTxO.TxAttributes
createTxAttributes txAdditionalSize =
  case txAdditionalSize of
    Nothing -> emptyAttributes
    Just (TxAdditionalSize size) -> blobAttributes size
 where
  emptyAttributes :: CC.UTxO.TxAttributes
  emptyAttributes = CC.Common.mkAttributes ()

  blobAttributes :: Int -> CC.UTxO.TxAttributes
  blobAttributes aSize =
    emptyAttributes {
      CC.Common.attrRemain = CC.Common.UnparsedFields $
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

-- | Append a non-empty list to a list.
-- > appendr [1,2,3] (4 :| [5]) == 1 :| [2,3,4,5]
appendr :: [a] -> NonEmpty a -> NonEmpty a
appendr l nel = foldr NE.cons nel l

-- | ...
createTxAux
  :: TopLevelConfig ByronBlock
  -> CC.UTxO.Tx
  -> SigningKey
  -> CC.UTxO.ATxAux ByteString
createTxAux config tx (SigningKeyByron signingKey) = CC.UTxO.annotateTxAux $ CC.UTxO.mkTxAux tx witness
 where
  witness = pure $
      CC.UTxO.VKWitness
        (Crypto.toVerification signingKey)
        (Crypto.sign
          (byronProtocolMagicId (configBlock config))
          -- provide ProtocolMagicId so as not to calculate it every time
          Crypto.SignTx
          signingKey
          (CC.UTxO.TxSigData (Crypto.serializeCborHash tx))
        )

-----------------------------------------------------------------------------------------
-- Helpers for work with lovelaces.
-----------------------------------------------------------------------------------------

assumeBound :: Either CC.Common.LovelaceError CC.Common.Lovelace
            -> CC.Common.Lovelace
assumeBound (Left err) = panic $ T.pack ("TxGeneration: " ++ show err)
assumeBound (Right ll) = ll

subLovelace :: CC.Common.Lovelace -> CC.Common.Lovelace -> CC.Common.Lovelace
subLovelace a b = assumeBound $ CC.Common.subLovelace a b

addLovelace' :: Lovelace -> Lovelace -> Lovelace
addLovelace' (Lovelace a) (Lovelace b) = Lovelace $ a + b

subLovelace' :: Lovelace -> Lovelace -> Lovelace
subLovelace' (Lovelace a) (Lovelace b)
  | a >= b    = Lovelace $ a - b
  | otherwise = error $ mconcat
                [ "subLovelace:  tried to subtract ", show b, " from ", show a]

-----------------------------------------------------------------------------------------
-- | Run benchmark using top level tracers..
-----------------------------------------------------------------------------------------

-- | Please note that there's a difference between Cardano tx and fiscal tx:
--   1. Cardano tx is a transaction from Cardano blockchain's point of view.
--   2. Fiscal tx is a transaction from recipient's point of view.
--   So if one Cardano tx contains 10 outputs (with addresses of 10 recipients),
--   we have 1 Cardano tx and 10 fiscal txs.
runBenchmark
  :: forall blk
  .  (RunNode blk, BenchTraceConstraints  blk)
  => Params blk
  -> IOManager
  -> SocketPath
  -> SigningKey
  -> Address
  -> NonEmpty NodeAddress
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> NumberOfOutputsPerTx
  -> FeePerTx
  -> TPSRate
  -> InitCooldown
  -> Maybe TxAdditionalSize
  -> Set (TxDetails blk)
  -> ExceptT TxGenError IO ()
runBenchmark p
             iocp
             socketFp
             sourceKey
             recipientAddress
             targetNodeAddresses
             numOfTxs
             numOfInsPerTx
             numOfOutsPerTx
             txFee
             tpsRate
             (InitCooldown initCooldown)
             txAdditionalSize
             fundsWithGenesisMoney = do
  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator, phase 1: make enough available UTxO entries using: " <> (show fundsWithGenesisMoney :: String)
  fundsWithSufficientCoins <-
      createMoreFundCoins p
                          iocp
                          socketFp
                          sourceKey
                          txFee
                          numOfTxs
                          numOfInsPerTx
                          fundsWithGenesisMoney

  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator: waiting " ++ show initCooldown ++ "s *******"
  liftIO $ threadDelay (initCooldown*1000*1000)

  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator, phase 2: pay to recipients *******"
  let benchmarkTracers :: BenchmarkTxSubmitTracers IO blk
      benchmarkTracers = BenchmarkTracers
                           { trSendRecvConnect      = trConnect p
                           , trSendRecvTxSubmission = trSubmitMux p
                           }

  let localAddr :: Maybe Network.Socket.AddrInfo
      localAddr = Nothing

  remoteAddresses <- forM targetNodeAddresses $ \targetNodeAddress -> do
    let (anAddrFamily, targetNodeHost) =
          case unNodeHostAddress $ naHostAddress targetNodeAddress of
              Just (IP.IPv4 ipv4) -> (AF_INET,  show ipv4)
              Just (IP.IPv6 ipv6) -> (AF_INET6, show ipv6)
              _ -> panic "Target node's IP-address is undefined!"

    let targetNodePort = show $ naPort targetNodeAddress

    let hints :: AddrInfo
        hints = defaultHints
          { addrFlags      = [AI_PASSIVE]
          , addrFamily     = anAddrFamily
          , addrSocketType = Stream
          , addrCanonName  = Nothing
          }

    (remoteAddr:_) <- liftIO $ getAddrInfo (Just hints) (Just targetNodeHost) (Just targetNodePort)
    return remoteAddr

  -- Run generator.
  let numTargets :: Natural = fromIntegral $ NE.length targetNodeAddresses
  txs :: [GenTx blk] <-
           txGenerator
              p
              recipientAddress
              sourceKey
              txFee
              (NE.length targetNodeAddresses)
              numOfTxs
              numOfInsPerTx
              numOfOutsPerTx
              txAdditionalSize
              fundsWithSufficientCoins

  liftIO $ do
    traceWith (trTxSubmit p) . TraceBenchTxSubDebug
        $ "******* Tx generator, launching Tx peers:  " ++ show (NE.length remoteAddresses) ++ " of them"
    submission <- mkSubmission (trTxSubmit p) $
                    SubmissionParams
                    { spTps      = tpsRate
                    , spTargets  = numTargets
                    , spQueueLen = 32
                    }
    allAsyncs <- forM (zip [0..] $ NE.toList remoteAddresses) $
      \(i, remoteAddr) ->
        launchTxPeer
              (trTxSubmit p)
              benchmarkTracers
              (trN2N p)
              iocp
              (paramsConfig p)
              localAddr
              remoteAddr
              submission
              i
    tpsFeeder <- async $ tpsLimitedTxFeeder submission txs
    -- Wait for all threads to complete.
    mapM_ wait (tpsFeeder : allAsyncs)
    traceWith (trTxSubmit p) =<<
       TraceBenchTxSubSummary <$> mkSubmissionSummary submission

-- | At this moment 'sourceAddress' contains a huge amount of money (lets call it A).
--   Now we have to split this amount to N equal parts, as a result we'll have
--   N UTxO entries, and alltogether these entries will contain the same amount A.
--   E.g. (1 entry * 1000 ADA) -> (10 entries * 100 ADA).
--   Technically all splitting transactions will send money back to 'sourceAddress'.
createMoreFundCoins
  :: Params blk
  -> IOManager
  -> SocketPath
  -> SigningKey
  -> FeePerTx
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> Set (TxDetails blk)
  -> ExceptT TxGenError IO (Set (TxDetails blk))
createMoreFundCoins p@ParamsByron{}
                    iocp
                    socketFp
                    sourceKey
                    (FeePerTx txFee)
                    (NumberOfTxs numOfTxs)
                    (NumberOfInputsPerTx numOfInsPerTx)
                    fundsWithGenesisMoney = do
  let Lovelace feeRaw = Lovelace . fromIntegral $ txFee
      -- The number of splitting txout entries (corresponds to the number of all inputs we will need).
      numSplittingTxOuts    = numOfTxs * fromIntegral numOfInsPerTx
      numOutsPerSplittingTx = 60 :: Word64 -- near the upper bound so as not to exceed the tx size limit

  -- Now we have to find the first output with sufficient amount of money.
  -- But since we made only one single transaction, there is only one
  -- entry in 'availableFunds', and it definitely contains a
  -- huge amount of money.
  let txDs@(TxDetailsByron (_, txo)) = Set.toList fundsWithGenesisMoney !! 0
      sourceAddress = AddressByron $ CC.UTxO.txOutAddress txo
      Lovelace rawSrcValue = fromByronLovelace $ CC.UTxO.txOutValue txo
      -- Split the funds to 'numSplittingTxOuts' equal parts, subtracting the possible fees.
      -- a safe number for fees is numSplittingTxOuts * feePerTx.
      splitValue = Lovelace $
                     ceiling (
                       (fromIntegral rawSrcValue :: Double)
                       /
                       (fromIntegral numSplittingTxOuts :: Double)
                     ) - feeRaw
      -- The same output for all splitting transaction: send the same 'splitValue'
      -- to the same 'sourceAddress'.
      !txOut        = TxOut sourceAddress splitValue
      -- Create and sign splitting txs.
      splittingTxs  = createSplittingTxs p
                                         (txDs, sourceKey)
                                         numSplittingTxOuts
                                         numOutsPerSplittingTx
                                         42
                                         txOut
                                         []
  -- Submit all splitting transactions sequentially.
  liftIO $ forM_ splittingTxs $ \(splittingTx, _) -> do
    submitTx' p iocp socketFp splittingTx

  -- Re-create availableFunds with information about all splitting transactions
  -- (it will be used for main transactions).
  right $ reCreateAvailableFunds splittingTxs
 where
  -- create txs which split the funds to numTxOuts equal parts
  createSplittingTxs
    :: Params ByronBlock
    -> (TxDetailsByron, SigningKey)
    -> Word64
    -> Word64
    -> Int
    -> TxOut
    -> [(GenTx ByronBlock, [TxDetailsByron])]
    -> [(GenTx ByronBlock, [TxDetailsByron])]
  createSplittingTxs parms@ParamsByron{} (details, sKey) numTxOuts maxOutsPerInitTx identityIndex txOut acc
    | numTxOuts <= 0 = reverse acc
    | otherwise =
        let numOutsPerInitTx = min maxOutsPerInitTx numTxOuts
            AddressByron sourceAddr = txdTxOutAddr details
            -- same TxOut for all
            outs = Set.fromList $ zip [identityIndex .. identityIndex + (fromIntegral numOutsPerInitTx) - 1]
                                      (repeat txOut)
            (mFunds, _fees, outIndices, genTx) =
              mkTransaction
                parms
                ((details, sKey) :| [])
                Nothing
                outs
                Nothing
                txFee
            !txId = getTxIdFromGenTxByron genTx
            txDetailsList = (flip map) (Map.toList outIndices) $
                \(_, txInIndex) ->
                  let !txIn  = CC.UTxO.TxInUtxo txId txInIndex
                  in TxDetailsByron (txIn, toByronTxOut txOut)
        in
          case mFunds of
            Nothing                 -> reverse $ (genTx, txDetailsList) : acc
            Just (txInIndex, value) ->
              let !txInChange  = CC.UTxO.TxInUtxo (getTxIdFromGenTxByron genTx) txInIndex
                  !txOutChange = CC.UTxO.TxOut
                                   { CC.UTxO.txOutAddress = sourceAddr
                                   , CC.UTxO.txOutValue   = toByronLovelace value
                                   }
                  details' = (TxDetailsByron (txInChange, txOutChange), sKey)
              in
                -- from the change create the next tx with numOutsPerInitTx UTxO entries
                createSplittingTxs p
                                   details'
                                   (numTxOuts - numOutsPerInitTx)
                                   numOutsPerInitTx
                                   (identityIndex + fromIntegral numOutsPerInitTx)
                                   txOut
                                   ((genTx, txDetailsList) : acc)
  reCreateAvailableFunds
    :: [(GenTx blk, [TxDetails blk])]
    -> Set (TxDetails blk)
  reCreateAvailableFunds =
    Set.fromList . concat . map snd

createMoreFundCoins p@ParamsShelley{}
                    _iocp
                    socketFp
                    srcKey
                    (FeePerTx txFee)
                    (NumberOfTxs numOfTxs)
                    (NumberOfInputsPerTx numOfInsPerTx)
                    fundsWithGenesisMoney = do
  let Lovelace feeRaw = Lovelace . fromIntegral $ txFee
      -- The number of splitting txout entries (corresponds to the number of all inputs we will need).
      numSplittingTxOuts    = numOfTxs * fromIntegral numOfInsPerTx
      numOutsPerSplittingTx = 45 :: Word64 -- near the upper bound so as not to exceed the tx size limit

  -- Now we have to find the first output with sufficient amount of money.
  -- But since we made only one single transaction, there is only one
  -- entry in 'availableFunds', and it definitely contains a
  -- huge amount of money.
  let txDs@(TxDetailsShelley (_, TxOut srcAddr (Lovelace rawSrcValue)))
        = Set.toList fundsWithGenesisMoney !! 0
      -- Split the funds to 'numSplittingTxOuts' equal parts, subtracting the possible fees.
      -- a safe number for fees is numSplittingTxOuts * feePerTx.
      splitValue = Lovelace $
                     ceiling ((fromIntegral rawSrcValue :: Double)
                              /
                              (fromIntegral numSplittingTxOuts :: Double)
                     ) - feeRaw
      -- The same output for all splitting transaction: send 'splitValue'
      -- back to 'srcAddress'.
      !txOut        = TxOut srcAddr splitValue
      -- Create and sign splitting txs.
      splittingTxs  = createSplittingTxs p
                                         (txDs, srcKey)
                                         numSplittingTxOuts
                                         numOutsPerSplittingTx
                                         42
                                         txOut
                                         []
      network :: Network
      network = paramsNetwork p
  -- Submit all splitting transactions sequentially.
  forM_ (zip splittingTxs [(0 :: Int) ..]) $
    \((Shelley.ShelleyTx _ tx, _), i) -> do
      r <- liftIO $ submitTx network socketFp (TxSignedShelley tx)
      case r of
        TxSubmitSuccess -> pure ()
        x -> left . SplittingSubmissionError $ mconcat
             ["Coin splitting submission failed (", show i :: Text
             , "/", show (numOfTxs - 1) :: Text
             , "): ", show x :: Text]

  -- Re-create availableFunds with information about all splitting transactions
  -- (it will be used for main transactions).
  right $ reCreateAvailableFunds splittingTxs
 where
  -- create txs which split the funds to numTxOuts equal parts
  createSplittingTxs
    :: Params ShelleyBlock
    -> (TxDetailsShelley, SigningKey)
    -> Word64
    -> Word64
    -> Int
    -> TxOut
    -> [(GenTx ShelleyBlock, [TxDetailsShelley])]
    -> [(GenTx ShelleyBlock, [TxDetailsShelley])]
  createSplittingTxs parms@ParamsShelley{} (details, sKey) numTxOuts maxOutsPerInitTx identityIndex txOut acc
    | numTxOuts <= 0 = reverse acc
    | otherwise =
        let numOutsPerInitTx = min maxOutsPerInitTx numTxOuts
            srcAddress = txdTxOutAddr details
            -- same TxOut for all
            outs = Set.fromList $ zip [identityIndex .. identityIndex + (fromIntegral numOutsPerInitTx) - 1]
                                      (repeat txOut)
            (mFunds, _fees, outIndices, genTx) =
              mkTransaction
                parms
                ((details, sKey) :| [])
                Nothing
                outs
                Nothing
                txFee
            !txId = getTxIdFromGenTxShelley genTx
            txDetailsList = (flip map) (Map.toList outIndices) $
                \(_, txInIndex) ->
                  let !txIn  = TxIn txId (fromIntegral txInIndex)
                  in TxDetailsShelley (txIn, txOut)
        in
          case mFunds of
            Nothing                 -> reverse $ (genTx, txDetailsList) : acc
            Just (txInIndex, value) ->
              let !txInChange  = TxIn (getTxIdFromGenTxShelley genTx) (fromIntegral txInIndex)
                  !txOutChange = TxOut srcAddress value
                  details' = (TxDetailsShelley (txInChange, txOutChange), sKey)
              in
                -- from the change create the next tx with numOutsPerInitTx UTxO entries
                createSplittingTxs p
                                   details'
                                   (numTxOuts - numOutsPerInitTx)
                                   numOutsPerInitTx
                                   (identityIndex + fromIntegral numOutsPerInitTx)
                                   txOut
                                   ((genTx, txDetailsList) : acc)
  reCreateAvailableFunds
    :: [(GenTx blk, [TxDetails blk])]
    -> Set (TxDetails blk)
  reCreateAvailableFunds =
    Set.fromList . concat . map snd

-----------------------------------------------------------------------------------------
-- | Work with tx generator thread (for Phase 2).
-----------------------------------------------------------------------------------------
txGenerator
  :: forall blk
  .  Params blk
  -> Address
  -> SigningKey
  -> FeePerTx
  -> Int
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> NumberOfOutputsPerTx
  -> Maybe TxAdditionalSize
  -> Set (TxDetails blk)
  -> ExceptT TxGenError IO [GenTx blk]
txGenerator p
            recipientAddress
            sourceKey
            (FeePerTx txFee)
            numOfTargetNodes
            (NumberOfTxs numOfTransactions)
            (NumberOfInputsPerTx numOfInsPerTx)
            (NumberOfOutputsPerTx numOfOutsPerTx)
            txAdditionalSize
            fundsWithSufficientCoins = do
  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ " Generating " ++ show numOfTransactions
      ++ " transactions, for " ++ show numOfTargetNodes ++ " peers"
  txs <- createMainTxs numOfTransactions numOfInsPerTx fundsWithSufficientCoins
  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ " Done, " ++ show numOfTransactions ++ " were generated."
  pure txs
 where
  -- Num of recipients is equal to 'numOuts', so we think of
  -- recipients as the people we're going to pay to.
  recipients = Set.fromList $ zip [initRecipientIndex .. initRecipientIndex + numOfOutsPerTx - 1]
                                  (repeat txOut)
  initRecipientIndex = 0 :: Int
  -- The same output for all transactions.
  !txOut = TxOut recipientAddress valueForRecipient
  valueForRecipient = Lovelace 100000000 -- 100 ADA, discuss this value.
  totalValue = valueForRecipient `addLovelace'` txFeeInLovelaces
  txFeeInLovelaces = Lovelace $ fromIntegral txFee
  -- Send possible change to the same 'recipientAddress'.
  addressForChange = recipientAddress

  -- Create all main transactions, using available funds.
  createMainTxs
    :: Word64
    -> Int
    -> Set (TxDetails blk)
    -> ExceptT TxGenError IO [GenTx blk]
  createMainTxs 0 _ _ = right []
  createMainTxs txsNum insNumPerTx funds = do
    (txInputs, updatedFunds) <- getTxInputs insNumPerTx funds
    let (_, _, _, txAux :: GenTx blk) =
          mkTransaction
            p
            (NE.fromList txInputs)
            (Just addressForChange)
            recipients
            txAdditionalSize
            txFee
    (txAux :) <$> createMainTxs (txsNum - 1) insNumPerTx updatedFunds

  -- Get inputs for one main transaction, using available funds.
  getTxInputs
    :: Int
    -> Set (TxDetails blk)
    -> ExceptT TxGenError IO ( [(TxDetails blk, SigningKey)]
                             , Set (TxDetails blk)
                             )
  getTxInputs 0 funds = right ([], funds)
  getTxInputs insNumPerTx funds = do
    (found, updatedFunds) <- findAvailableFunds funds totalValue
    (inputs, updatedFunds') <- getTxInputs (insNumPerTx - 1) updatedFunds
    right ((found, sourceKey) : inputs, updatedFunds')

  -- Find a source of available funds, removing it from the availableFunds
  -- for preventing of double spending.
  findAvailableFunds
    :: Set (TxDetails blk)     -- funds we are trying to find in
    -> Lovelace                -- with at least this associated value
    -> ExceptT TxGenError IO (TxDetails blk, Set (TxDetails blk))
  findAvailableFunds funds thresh =
    case find (predTxD thresh) funds of
      Nothing    -> left InsufficientFundsForRecipientTx
      Just found -> right (found, Set.delete found funds)

  -- Find the first tx output that contains sufficient amount of money.
  predTxD :: Lovelace -> TxDetails blk -> Bool
  predTxD valueThreshold txd = txdTxOutValue txd >= valueThreshold

---------------------------------------------------------------------------------------------------
-- Txs for submission.
---------------------------------------------------------------------------------------------------

-- | To get higher performance we need to hide latency of getting and
-- forwarding (in sufficient numbers) transactions.
--
-- TODO: transform comments into haddocks.
--
launchTxPeer
  :: forall block tx txid.
     ( tx ~ GenTx block
     , txid ~ Mempool.GenTxId block
     , RunNode block
     )
  => Tracer IO (TraceBenchTxSubmit txid)
  -> BenchmarkTxSubmitTracers IO block
  -- tracer for lower level connection and details of
  -- protocol interactisn, intended for debugging
  -- associated issues.
  -> Tracer IO NodeToNodeSubmissionTrace
  -> IOManager
  -- ^ associate a file descriptor with IO completion port
  -> TopLevelConfig block
  -- the configuration
  -> Maybe Network.Socket.AddrInfo
  -- local address binding (if wanted)
  -> Network.Socket.AddrInfo
  -- Remote address
  -> Submission IO block
  -- Mutable state shared between submission threads
  -> Natural
  -- Thread index
  -> IO (Async ())
launchTxPeer tr1 tr2 tr3 iocp nc localAddr remoteAddr ss ix = do
  async $
    benchmarkConnectTxSubmit iocp tr2 nc localAddr remoteAddr
      (txSubmissionClient tr1 tr3 ss ix)
