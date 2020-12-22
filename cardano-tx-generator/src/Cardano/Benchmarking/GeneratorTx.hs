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
{-# LANGUAGE TypeApplications #-}

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

import           Control.Monad (fail)
import           Control.Monad.Trans.Except.Extra (left, newExceptT, right)
import           Control.Tracer (Tracer, traceWith)

import qualified Data.Aeson as A
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Text (pack)
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Family (..), SocketType (Stream),
                                 addrFamily, addrFlags, addrSocketType, defaultHints, getAddrInfo)

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

import           Shelley.Spec.Ledger.API (ShelleyGenesis)

readSigningKey :: SigningKeyFile -> ExceptT TxGenError IO (SigningKey PaymentKey)
readSigningKey =
  withExceptT TxFileError . newExceptT . readKey . unSigningKeyFile
 where
   readKey :: FilePath -> IO (Either (FileError TextEnvelopeError) (SigningKey PaymentKey))
   readKey f = flip readFileTextEnvelopeAnyOf f
     [ FromSomeType (AsSigningKey AsGenesisUTxOKey) castSigningKey
     , FromSomeType (AsSigningKey AsPaymentKey) id
     ]

secureFunds :: forall era. IsShelleyBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> LocalNodeConnectInfo CardanoMode CardanoBlock
  -> Benchmark
  -> NetworkId
  -> ShelleyGenesis StandardShelley
  -> GeneratorFunds
  -> ExceptT TxGenError IO (SigningKey PaymentKey, [(TxIn, TxOut era)])
secureFunds submitTracer localConnectInfo benchmark networkId genesis funds = case funds of
  FundsGenesis keyF -> do
    let Benchmark{bTxFee, bInitialTTL, bInitCooldown=InitCooldown cooldown} = benchmark
    key <- readSigningKey keyF
    let (_inAddr, lovelace) = genesisFundForKey @ era networkId genesis key
        toAddr = keyAddress networkId key
        (tx, txin, txout) =
           genesisExpenditure networkId key toAddr lovelace bTxFee bInitialTTL
    r <- liftIO $ submitTx localConnectInfo (TxForCardanoMode $ InAnyCardanoEra cardanoEra tx)
    case r of
      TxSubmitSuccess ->
        liftIO . traceWith submitTracer . TraceBenchTxSubDebug
        $ mconcat
        [ "******* Funding secured (", show txin, " -> ", show txout
        , "), submission result: " , show r ]
      e -> fail $ show e
    liftIO $ threadDelay (cooldown*1000*1000)
    (key, ) <$> splitFunds submitTracer localConnectInfo benchmark key (txin, txout)

{-
  FundsUtxo keyF txin txout -> do
    key <- readSigningKey keyF
    (key, ) <$> splitFunds benchmark m key (txin, txout)

  FundsSplitUtxo keyF utxoF -> do
    let Benchmark{bTxCount=NumberOfTxs (fromIntegral -> ntxs)} = benchmark
    key <- readSigningKey keyF
    utxo <- withExceptT (UtxoReadFailure . pack) . newExceptT $ A.eitherDecode <$> BS.readFile utxoF
    when (length utxo < ntxs) $
      left $ SuppliedUtxoTooSmall ntxs (length utxo)
    pure (key, utxo)
-}

parseAddress ::
   (IsShelleyBasedEra era)
  => Text
  -> AddressInEra era
parseAddress addr=
    fromMaybe (panic $ "Bad address: " <> addr) $
    shelleyAddressInEra <$>
    deserialiseAddress AsShelleyAddress addr

instance IsShelleyBasedEra era => FromJSON (AddressInEra era) where
  parseJSON = A.withText "ShelleyAddress" $ pure . parseAddress

--data Era era where
--  EraShelley :: Era ShelleyEra

instance (IsShelleyBasedEra era, FromJSON (AddressInEra era)) => FromJSON (TxOut era) where
  parseJSON = A.withObject "TxOut" $ \v ->
    TxOut
      <$> (v A..: "addr")
      <*> (v A..: "coin"
           <&> mkTxOutValueAdaOnly . Lovelace)

instance FromJSON TxIn where
  parseJSON = A.withObject "TxIn" $ \v -> do
    TxIn
      <$> (TxId <$> v A..: "txid")
      <*> (TxIx <$> v A..: "txix")

-----------------------------------------------------------------------------------------
-- Obtain initial funds.
-----------------------------------------------------------------------------------------
splitFunds  :: forall era. IsShelleyBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> LocalNodeConnectInfo CardanoMode CardanoBlock
  -> Benchmark
  -> SigningKey PaymentKey
  -> (TxIn, TxOut era)
  -> ExceptT TxGenError IO [(TxIn, TxOut era)]
splitFunds
    submitTracer
    localConnInfo
    Benchmark{ bTxFee=fee@(Lovelace feeRaw), bTxCount=NumberOfTxs numTxs
             , bTxFanIn=NumberOfInputsPerTx txFanin
             }
    sourceKey fundsTxIO@(_, (TxOut addr value)) = do
  let -- The number of splitting txout entries (corresponds to the number of all inputs we will need).
      (Lovelace rawCoin) = txOutValueToLovelace value
      numRequiredTxOuts = numTxs * fromIntegral txFanin
      splitFanout = 60 :: Word64 -- near the upper bound so as not to exceed the tx size limit
      (nFullTxs, remainder) = numRequiredTxOuts `divMod` splitFanout
      numSplitTxs = nFullTxs + if remainder > 0 then 1 else 0

  let -- Split the funds to 'numRequiredTxOuts' equal parts, subtracting the possible fees.
      -- a safe number for fees is numRequiredTxOuts' * feePerTx.
      outputSliceWithFees = ceiling $
        (fromIntegral rawCoin :: Double)
        /
        (fromIntegral numRequiredTxOuts :: Double)
      outputSlice = outputSliceWithFees - feeRaw
      splitValue = mkTxOutValueAdaOnly $ Lovelace outputSlice
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
  liftIO $ traceWith submitTracer $ TraceBenchTxSubDebug $ mconcat
     [ "Coin splitting (values are Lovelaces): "
     , "total funds: ", show rawCoin, ", "
     , "txouts needed: ", show numRequiredTxOuts, ", "
     , "txout slice with fees: ", show outputSliceWithFees, ", "
     , "fees: ", show feeRaw
     , "txout slice: ", show outputSlice
     , "splitting fanout: ", show splitFanout
     , "splitting tx count: ", show (length splittingTxs)
     ]
  forM_ (zip splittingTxs [0::Int ..]) $ \((tx, _), i) ->
    liftIO (submitTx localConnInfo (TxForCardanoMode $ InAnyCardanoEra cardanoEra tx))
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
    :: SigningKey PaymentKey
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
              mkTransactionGen sKey (txIO :| []) Nothing outs 0 fee
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
                  !txOutChange = TxOut srcAddr (mkTxOutValueAdaOnly value)
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
runBenchmark :: forall era .
    (ConfigSupportsTxGen CardanoMode era, IsShelleyBasedEra era)
  => Benchmark
  -> Mode
  -> (SigningKey PaymentKey, [(TxIn, TxOut era)])
  -> ExceptT TxGenError IO ()
runBenchmark b@Benchmark{ bTargets
                        , bTps
                        , bInitCooldown=InitCooldown initCooldown
                        }
             m (fundsKey, fundsWithSufficientCoins) = do
  let
    networkId = modeNetworkIdOverridable m
    recipientAddress = keyAddress networkId fundsKey

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
    submission :: Submission IO era  <- mkSubmission (trTxSubmit m) $
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
  :: forall era
  .  (ConfigSupportsTxGen CardanoMode era, IsShelleyBasedEra era)
  => Benchmark
  -> Mode
  -> AddressInEra era
  -> SigningKey PaymentKey
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
  !txOut = TxOut recipientAddress (mkTxOutValueAdaOnly valueForRecipient)
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
  predTxD valueThreshold (_, TxOut _ value) = txOutValueToLovelace value >= valueThreshold

---------------------------------------------------------------------------------------------------
-- Txs for submission.
---------------------------------------------------------------------------------------------------

-- | To get higher performance we need to hide latency of getting and
-- forwarding (in sufficient numbers) transactions.
--
-- TODO: transform comments into haddocks.
--
launchTxPeer
  :: forall era
  .  (ConfigSupportsTxGen CardanoMode era, IsShelleyBasedEra era)
  => Mode
  -> Maybe Network.Socket.AddrInfo
  -- local address binding (if wanted)
  -> Network.Socket.AddrInfo
  -- Remote address
  -> Submission IO  era
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
        (txSubmissionClient (trN2N m) (trTxSubmit m) sub tix)
