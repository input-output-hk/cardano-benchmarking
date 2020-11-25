{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Benchmarking.GeneratorTx
  ( NumberOfTxs(..)
  , NumberOfInputsPerTx(..)
  , NumberOfOutputsPerTx(..)
  , InitCooldown(..)
  , TPSRate(..)
  , TxAdditionalSize(..)
  , TxGenError
  , secureFunds
  , runBenchmark
  ) where

import           Cardano.Prelude
import           Prelude (error, id)

import           Control.Concurrent (threadDelay)
import           Control.Monad (fail, forM, forM_)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left, newExceptT, right)
import           Control.Tracer (traceWith)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import           Data.Foldable (find)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Maybe (Maybe (..))
import           Data.Text (Text, pack)
import           Data.Word (Word64)
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Family (..), SocketType (Stream),
                                 addrFamily, addrFlags, addrSocketType, defaultHints, getAddrInfo)

import           Cardano.Chain.Common (decodeAddressBase58)
import           Cardano.CLI.Types (SigningKeyFile (..))
import           Cardano.Node.Types

import           Cardano.Api.TxSubmit
import           Cardano.Api.Typed

import           Cardano.Benchmarking.GeneratorTx.Benchmark
import           Cardano.Benchmarking.GeneratorTx.Era
import           Cardano.Benchmarking.GeneratorTx.Error
import           Cardano.Benchmarking.GeneratorTx.Genesis
import           Cardano.Benchmarking.GeneratorTx.NodeToNode
import           Cardano.Benchmarking.GeneratorTx.Submission
import           Cardano.Benchmarking.GeneratorTx.Tx


readSigningKey ::
     Era era -> SigningKeyFile
  -> ExceptT TxGenError IO (SigningKeyOf era)
readSigningKey p =
  withExceptT TxFileError . newExceptT . readKey p . unSigningKeyFile
 where
   readKey :: Era era -> FilePath -> IO (Either (FileError TextEnvelopeError) (SigningKeyOf era))
   readKey EraByron{}   f = flip readFileTextEnvelopeAnyOf f
     [ FromSomeType (AsSigningKey AsByronKey) id]
   readKey EraShelley{} f = flip readFileTextEnvelopeAnyOf f
     [ FromSomeType (AsSigningKey AsGenesisUTxOKey) castSigningKey
     , FromSomeType (AsSigningKey AsPaymentKey) id
     ]

secureFunds :: ConfigSupportsTxGen mode era
  => Benchmark
  -> Mode mode era
  -> GeneratorFunds
  -> ExceptT TxGenError IO (SigningKeyOf era, [(TxIn, TxOut era)])

secureFunds b@Benchmark{bTxFee, bInitialTTL, bInitCooldown=InitCooldown cooldown} m
 (FundsGenesis keyF) = do
  key <- readSigningKey (modeEra m) keyF
  let (_, TxOut _ (txOutValueLovelace -> genesisCoin)) = extractGenesisFunds m key
      toAddr = keyAddress m modeNetworkId key
      (tx, txin, txout) =
         genesisExpenditure m key toAddr genesisCoin bTxFee bInitialTTL
      txOfMode = castTxMode m tx
  r <- liftIO $ submitTx (modeLocalConnInfo m) txOfMode
  case r of
    TxSubmitSuccess ->
      liftIO . traceWith (trTxSubmit m) . TraceBenchTxSubDebug
      $ mconcat
      [ "******* Funding secured (", show txin, " -> ", show txout
      , "), submission result: " , show r ]
    e -> fail $ show e
  liftIO $ threadDelay (cooldown*1000*1000)
  (key, ) <$> splitFunds b m key (txin, txout)

secureFunds b m@ModeShelley{} (FundsUtxo keyF txin txout) = do
  key <- readSigningKey (modeEra m) keyF
  (key, ) <$> splitFunds b m key (txin, txout)

secureFunds b m@ModeCardanoShelley{} (FundsUtxo keyF txin txout) = do
  key <- readSigningKey (modeEra m) keyF
  (key, ) <$> splitFunds b m key (txin, txout)

secureFunds Benchmark{bTxCount=NumberOfTxs (fromIntegral -> ntxs)}
            m (FundsSplitUtxo keyF utxoF) = do
  key <- readSigningKey (modeEra m) keyF
  utxo <- withExceptT (UtxoReadFailure . pack) . newExceptT $ A.eitherDecode <$> BS.readFile utxoF
  when (length utxo < ntxs) $
    left $ SuppliedUtxoTooSmall ntxs (length utxo)
  pure (key, utxo)

secureFunds _ m f =
  error $ "secureFunds:  unsupported config: " <> show m <> " / " <> show f

-- https://github.com/input-output-hk/cardano-node/issues/1857
parseAddress ::
     Era era
  -> Text
  -> AddressInEra era
parseAddress = \case
  EraByron ->
    either (panic . ("Bad Base58 address: " <>) . show)
           (byronAddressInEra . ByronAddress)
    . decodeAddressBase58
  EraShelley -> \addr ->
    fromMaybe (panic $ "Bad Shelley address: " <> addr) $
    shelleyAddressInEra <$>
    deserialiseAddress AsShelleyAddress addr

instance FromJSON (AddressInEra Byron) where
  parseJSON = A.withText "ByronAddress" $ pure . parseAddress EraByron

instance FromJSON (AddressInEra Shelley) where
  parseJSON = A.withText "ShelleyAddress" $ pure . parseAddress EraShelley

instance (FromJSON (AddressInEra era), ReifyEra era) => FromJSON (TxOut era) where
  parseJSON = A.withObject "TxOut" $ \v ->
    TxOut
      <$> (v A..: "addr")
      <*> (v A..: "coin"
           <&> mkTxOutValueAdaOnly reifyEra . Lovelace)

instance FromJSON TxIn where
  parseJSON = A.withObject "TxIn" $ \v -> do
    TxIn
      <$> (TxId <$> v A..: "txid")
      <*> (TxIx <$> v A..: "txix")

-----------------------------------------------------------------------------------------
-- Obtain initial funds.
-----------------------------------------------------------------------------------------
splitFunds
  :: forall mode era
  .  ConfigSupportsTxGen mode era
  => Benchmark
  -> Mode mode era
  -> SigningKeyOf era
  -> (TxIn, TxOut era)
  -> ExceptT TxGenError IO [(TxIn, TxOut era)]
splitFunds
    Benchmark{ bTxFee=fee@(Lovelace feeRaw), bTxCount=NumberOfTxs numTxs
             , bTxFanIn=NumberOfInputsPerTx txFanin
             }
    m sourceKey fundsTxIO@(_, (TxOut addr (TxOutAdaOnly _ (Lovelace rawCoin)))) = do
  let -- The number of splitting txout entries (corresponds to the number of all inputs we will need).
      numRequiredTxOuts = numTxs * fromIntegral txFanin
      splitFanout = 60 :: Word64 -- near the upper bound so as not to exceed the tx size limit
      (nFullTxs, remainder) = numRequiredTxOuts `divMod` splitFanout
      numSplitTxs = nFullTxs + if remainder > 0 then 1 else 0

  let -- Split the funds to 'numRequiredTxOuts' equal parts, subtracting the possible fees.
      -- a safe number for fees is numRequiredTxOuts' * feePerTx.
      splitValue = mkTxOutValueAdaOnly (modeEra m) $ Lovelace $
                     ceiling (
                       (fromIntegral rawCoin :: Double)
                       /
                       (fromIntegral numRequiredTxOuts :: Double)
                     ) - feeRaw
      -- The same output for all splitting transaction: send the same 'splitValue'
      -- to the same 'sourceAddress'.
      !txOut        = TxOut addr splitValue
      -- Create and sign splitting txs.
      splittingTxs  = createSplittingTxs sourceKey
                                         fundsTxIO
                                         numRequiredTxOuts
                                         splitFanout
                                         42
                                         txOut
                                         []
  -- Submit all splitting transactions sequentially.
  forM_ (zip splittingTxs [0::Int ..]) $ \((tx, _), i) ->
    liftIO (submitTx (modeLocalConnInfo m) (castTxMode m tx))
    >>= \case
      TxSubmitSuccess -> pure ()
      x -> left . SplittingSubmissionError $ mconcat
           ["Coin splitting submission failed (", show i :: Text
           , "/", show numSplitTxs :: Text
           , "): ", show x :: Text
           , "\n  Tx: ", show tx]
  liftIO $ putStrLn ("submitted all coin splitting Txs." :: Text)

  -- Re-create availableFunds with information about all splitting transactions
  -- (it will be used for main transactions).
  right $ concatMap snd splittingTxs
 where
  -- create txs which split the funds to numTxOuts equal parts
  createSplittingTxs
    :: SigningKeyOf era
    -> (TxIn, TxOut era)
    -> Word64
    -> Word64
    -> Int
    -> TxOut era
    -> [(Tx era, [(TxIn, TxOut era)])]
    -> [(Tx era, [(TxIn, TxOut era)])]
  createSplittingTxs sKey txIO@(_, TxOut srcAddr _) numTxOuts maxOutsPerInitTx identityIndex txOut acc
    | numTxOuts <= 0 = reverse acc
    | otherwise =
        let numOutsPerInitTx = min maxOutsPerInitTx numTxOuts
            -- same TxOut for all
            outs = zip [identityIndex ..
                        identityIndex + fromIntegral numOutsPerInitTx - 1]
                       (repeat txOut)
            (mFunds, _fees, outIndices, splitTx) =
              mkTransactionGen m sKey (txIO :| []) Nothing outs 0 fee
            !splitTxId = getTxId $ getTxBody splitTx
            txIOList = flip map (Map.toList outIndices) $
                \(_, txInIndex) ->
                  let !txIn  = TxIn splitTxId txInIndex
                  in (txIn, txOut)
        in
          case mFunds of
            Nothing                 -> reverse $ (splitTx, txIOList) : acc
            Just (txInIndex, value) ->
              let !txInChange  = TxIn splitTxId txInIndex
                  !txOutChange = TxOut srcAddr (mkTxOutValueAdaOnly (modeEra m) value)
              in
                -- from the change create the next tx with numOutsPerInitTx UTxO entries
                createSplittingTxs sKey
                                   (txInChange, txOutChange)
                                   (numTxOuts - numOutsPerInitTx)
                                   numOutsPerInitTx
                                   (identityIndex + fromIntegral numOutsPerInitTx)
                                   txOut
                                   ((splitTx, txIOList) : acc)

-----------------------------------------------------------------------------------------
-- | Run benchmark using top level tracers..
-----------------------------------------------------------------------------------------

-- | Please note that there's a difference between Cardano tx and fiscal tx:
--   1. Cardano tx is a transaction from Cardano blockchain's point of view.
--   2. Fiscal tx is a transaction from recipient's point of view.
--   So if one Cardano tx contains 10 outputs (with addresses of 10 recipients),
--   we have 1 Cardano tx and 10 fiscal txs.
runBenchmark
  :: forall mode era
  .  ConfigSupportsTxGen mode era
  => Benchmark
  -> Mode mode era
  -> SigningKeyOf era
  -> [(TxIn, TxOut era)]
  -> ExceptT TxGenError IO ()
runBenchmark b@Benchmark{ bTargets
                        , bTps
                        , bInitCooldown=InitCooldown initCooldown
                        }
             m fundsKey fundsWithSufficientCoins = do
  let recipientAddress = keyAddress m modeNetworkIdOverridable fundsKey

  liftIO . traceWith (trTxSubmit m) . TraceBenchTxSubDebug
    $ "******* Tx generator: waiting " ++ show initCooldown ++ "s *******"
  liftIO $ threadDelay (initCooldown*1000*1000)

  liftIO . traceWith (trTxSubmit m) . TraceBenchTxSubDebug
    $ "******* Tx generator, phase 2: pay to recipients *******"

  let localAddr :: Maybe Network.Socket.AddrInfo
      localAddr = Nothing

  remoteAddresses <- forM bTargets $ \targetNodeAddress -> do
    let targetNodeHost =
          show . unNodeHostIPv4Address $ naHostAddress targetNodeAddress

    let targetNodePort = show $ naPort targetNodeAddress

    let hints :: AddrInfo
        hints = defaultHints
          { addrFlags      = [AI_PASSIVE]
          , addrFamily     = AF_INET
          , addrSocketType = Stream
          , addrCanonName  = Nothing
          }

    (remoteAddr:_) <- liftIO $ getAddrInfo (Just hints) (Just targetNodeHost) (Just targetNodePort)
    return remoteAddr

  -- Run generator.
  let numTargets :: Natural = fromIntegral $ NE.length bTargets
  txs :: [Tx era] <-
           txGenerator
              b m
              recipientAddress
              fundsKey
              (NE.length bTargets)
              fundsWithSufficientCoins

  liftIO $ do
    traceWith (trTxSubmit m) . TraceBenchTxSubDebug
        $ "******* Tx generator, launching Tx peers:  " ++ show (NE.length remoteAddresses) ++ " of them"
    submission <- mkSubmission (trTxSubmit m) $
                    SubmissionParams
                    { spTps           = bTps
                    , spTargets       = numTargets
                    , spQueueLen      = 32
                    , spErrorPolicy   = bErrorPolicy b
                    }
    allAsyncs <- forM (zip [0..] $ NE.toList remoteAddresses) $
      \(i, remoteAddr) ->
        launchTxPeer
              m
              localAddr
              remoteAddr
              submission
              i
    tpsFeeder <- async $ tpsLimitedTxFeeder submission txs
    -- Wait for all threads to complete.
    mapM_ wait (tpsFeeder : allAsyncs)
    traceWith (trTxSubmit m) =<<
       TraceBenchTxSubSummary <$> mkSubmissionSummary submission

-- | At this moment 'sourceAddress' contains a huge amount of money (lets call it A).
--   Now we have to split this amount to N equal parts, as a result we'll have
--   N UTxO entries, and alltogether these entries will contain the same amount A.
--   E.g. (1 entry * 1000 ADA) -> (10 entries * 100 ADA).
--   Technically all splitting transactions will send money back to 'sourceAddress'.

-----------------------------------------------------------------------------------------
-- | Work with tx generator thread (for Phase 2).
-----------------------------------------------------------------------------------------
txGenerator
  :: forall mode era
  .  ConfigSupportsTxGen mode era
  => Benchmark
  -> Mode mode era
  -> AddressInEra era
  -> SigningKeyOf era
  -> Int
  -> [(TxIn, TxOut era)]
  -> ExceptT TxGenError IO [Tx era]
txGenerator Benchmark
            { bTxFee
            , bTxCount=NumberOfTxs numOfTransactions
            , bTxFanIn=NumberOfInputsPerTx numOfInsPerTx
            , bTxFanOut=NumberOfOutputsPerTx numOfOutsPerTx
            , bTxExtraPayload=txAdditionalSize
            }
            m recipientAddress sourceKey numOfTargetNodes
            fundsWithSufficientCoins = do
  liftIO . traceWith (trTxSubmit m) . TraceBenchTxSubDebug
    $ " Generating " ++ show numOfTransactions
      ++ " transactions, for " ++ show numOfTargetNodes
      ++ " peers, fee " ++ show bTxFee
      ++ ", value " ++ show valueForRecipient
      ++ ", totalValue " ++ show totalValue
  txs <- createMainTxs numOfTransactions numOfInsPerTx fundsWithSufficientCoins
  liftIO . traceWith (trTxSubmit m) . TraceBenchTxSubDebug
    $ " Done, " ++ show numOfTransactions ++ " were generated."
  pure txs
 where
  -- Num of recipients is equal to 'numOuts', so we think of
  -- recipients as the people we're going to pay to.
  recipients = zip [initRecipientIndex .. initRecipientIndex + numOfOutsPerTx - 1]
                   (repeat txOut)
  initRecipientIndex = 0 :: Int
  -- The same output for all transactions.
  valueForRecipient = Lovelace 1000000 -- 10 ADA
  !txOut = TxOut recipientAddress (mkTxOutValueAdaOnly (modeEra m) valueForRecipient)
  totalValue = valueForRecipient + bTxFee
  -- Send possible change to the same 'recipientAddress'.
  addressForChange = recipientAddress

  -- Create all main transactions, using available funds.
  createMainTxs
    :: Word64
    -> Int
    -> [(TxIn, TxOut era)]
    -> ExceptT TxGenError IO [Tx era]
  createMainTxs 0 _ _ = right []
  createMainTxs txsNum insNumPerTx funds = do
    (txInputs, updatedFunds) <- getTxInputs insNumPerTx funds
    let (_, _, _, txAux :: Tx era) =
          mkTransactionGen
            m
            sourceKey
            (NE.fromList txInputs)
            (Just addressForChange)
            recipients
            txAdditionalSize
            bTxFee
    (txAux :) <$> createMainTxs (txsNum - 1) insNumPerTx updatedFunds

  -- Get inputs for one main transaction, using available funds.
  getTxInputs
    :: Int
    -> [(TxIn, TxOut era)]
    -> ExceptT TxGenError IO ( [(TxIn, TxOut era)]
                             , [(TxIn, TxOut era)]
                             )
  getTxInputs 0 funds = right ([], funds)
  getTxInputs insNumPerTx funds = do
    (found, updatedFunds) <- findAvailableFunds funds totalValue
    (inputs, updatedFunds') <- getTxInputs (insNumPerTx - 1) updatedFunds
    right (found : inputs, updatedFunds')

  -- Find a source of available funds, removing it from the availableFunds
  -- for preventing of double spending.
  findAvailableFunds
    :: [(TxIn, TxOut era)]     -- funds we are trying to find in
    -> Lovelace                -- with at least this associated value
    -> ExceptT TxGenError IO ((TxIn, TxOut era), [(TxIn, TxOut era)])
  findAvailableFunds funds thresh =
    case break (predTxD thresh) funds of
      (_, [])    ->
        left $ InsufficientFundsForRecipientTx
                 thresh
                 (maximum [ coin
                          | (_, TxOut _ (TxOutAdaOnly _ coin))
                            <- funds])
      (toofews, found:rest) -> right (found, toofews <> rest)

  -- Find the first tx output that contains sufficient amount of money.
  predTxD :: Lovelace -> (TxIn, TxOut era) -> Bool
  predTxD valueThreshold (_, TxOut _ (TxOutAdaOnly _ coin)) =
    coin >= valueThreshold

---------------------------------------------------------------------------------------------------
-- Txs for submission.
---------------------------------------------------------------------------------------------------

-- | To get higher performance we need to hide latency of getting and
-- forwarding (in sufficient numbers) transactions.
--
-- TODO: transform comments into haddocks.
--
launchTxPeer
  :: forall mode era
  .  ConfigSupportsTxGen mode era
  => Mode mode era
  -> Maybe Network.Socket.AddrInfo
  -- local address binding (if wanted)
  -> Network.Socket.AddrInfo
  -- Remote address
  -> Submission IO era
  -- Mutable state shared between submission threads
  -> Natural
  -- Thread index
  -> IO (Async ())
launchTxPeer m localAddr remoteAddr sub tix =
  async $
   handle
     (\(SomeException err) -> do
         let errDesc = mconcat
               [ "Exception while talking to peer #", show tix
               , " (", show (addrAddress remoteAddr), "): "
               , show err]
         submitThreadReport sub tix (Left errDesc)
         case spErrorPolicy $ sParams sub of
           FailOnError -> throwIO err
           LogErrors   -> traceWith (trTxSubmit m) $
             TraceBenchTxSubError (pack errDesc))
     $ benchmarkConnectTxSubmit m localAddr remoteAddr
        (txSubmissionClient m (trN2N m) (trTxSubmit m) sub tix)
