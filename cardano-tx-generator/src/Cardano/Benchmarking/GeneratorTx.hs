{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Benchmarking.GeneratorTx
  ( AsyncBenchmarkControl
  , NumberOfTxs(..)
  , NumberOfInputsPerTx(..)
  , NumberOfOutputsPerTx(..)
  , InitCooldown(..)
  , TPSRate(..)
  , TxAdditionalSize(..)
  , TxGenError
  , secureGenesisFund
  , splitFunds
  , runBenchmark
  , asyncBenchmark
  , waitBenchmark
  , readSigningKey
  , txGenerator
  ) where

import           Cardano.Prelude
import           Prelude (id, String)

import           Control.Monad (fail)
import           Control.Monad.Trans.Except.Extra (left, newExceptT, right)
import           Control.Tracer (Tracer, traceWith)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Text (pack)
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Family (..), SocketType (Stream),
                                 addrFamily, addrFlags, addrSocketType, defaultHints, getAddrInfo)

import           Cardano.CLI.Types (SigningKeyFile (..))
import           Cardano.Node.Types

import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)

import           Cardano.Api
import           Cardano.Api.Shelley (CardanoMode)

import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.GeneratorTx.Error
import           Cardano.Benchmarking.GeneratorTx.Genesis
import           Cardano.Benchmarking.GeneratorTx.NodeToNode
import           Cardano.Benchmarking.GeneratorTx.Submission
import           Cardano.Benchmarking.GeneratorTx.Tx
import           Cardano.Benchmarking.GeneratorTx.SizedMetadata (mkMetadata)
import           Cardano.Benchmarking.Tracer

import           Shelley.Spec.Ledger.API (ShelleyGenesis)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))

readSigningKey :: SigningKeyFile -> ExceptT TxGenError IO (SigningKey PaymentKey)
readSigningKey =
  withExceptT TxFileError . newExceptT . readKey . unSigningKeyFile
 where
   readKey :: FilePath -> IO (Either (FileError TextEnvelopeError) (SigningKey PaymentKey))
   readKey f = flip readFileTextEnvelopeAnyOf f
     [ FromSomeType (AsSigningKey AsGenesisUTxOKey) castSigningKey
     , FromSomeType (AsSigningKey AsPaymentKey) id
     ]

secureGenesisFund :: forall era. IsShelleyBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> (TxInMode CardanoMode -> IO (SubmitResult (TxValidationErrorInMode CardanoMode)))
  -> NetworkId
  -> ShelleyGenesis StandardShelley
  -> Lovelace
  -> SlotNo
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> ExceptT TxGenError IO Fund
secureGenesisFund submitTracer localSubmitTx networkId genesis txFee ttl key outAddr = do
    let (_inAddr, lovelace) = genesisFundForKey @ era networkId genesis key
        (tx, fund) =
           genesisExpenditure networkId key outAddr lovelace txFee ttl
    r <- liftIO $
      catches (localSubmitTx $ txInModeCardano tx)
        [ Handler $ \e@SomeException{} ->
            fail $ mconcat
              [ "Exception while moving genesis funds via local socket: "
              , show e
              ]]
    case r of
      SubmitSuccess ->
        liftIO . traceWith submitTracer . TraceBenchTxSubDebug
        $ mconcat
        [ "******* Funding secured ("
        , show $ fundTxIn fund, " -> ", show $ fundAdaValue fund
        , ")"]
      SubmitFail e -> fail $ show e
    return fund

-----------------------------------------------------------------------------------------
-- Obtain initial funds.
-----------------------------------------------------------------------------------------
splitFunds  :: forall era. IsShelleyBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> (TxInMode CardanoMode -> IO (SubmitResult (TxValidationErrorInMode CardanoMode)))
  -> Lovelace
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> (SigningKey PaymentKey)
  -> AddressInEra era
  -> Fund
  -> ExceptT TxGenError IO [Fund]
splitFunds
    submitTracer
    localSubmitTx
    fee
    (NumberOfTxs numTxs)
    (NumberOfInputsPerTx txFanin)
    sourceKey
    globalOutAddr
    fundsTxIO = do
  let -- The number of splitting txout entries (corresponds to the number of all inputs we will need).
      (Quantity rawCoin) = lovelaceToQuantity $ fundAdaValue fundsTxIO
      (Quantity feeRaw) = lovelaceToQuantity fee
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
      splitValue = mkTxOutValueAdaOnly $ quantityToLovelace $ Quantity outputSlice
      -- The same output for all splitting transaction: send the same 'splitValue'
      -- to the same 'sourceAddress'.
      -- Create and sign splitting txs.
      splittingTxs  = createSplittingTxs sourceKey
                                         fundsTxIO
                                         numRequiredTxOuts
                                         splitFanout
                                         42
                                         splitValue
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
    liftIO (localSubmitTx $ txInModeCardano tx)
    >>= \case
      SubmitSuccess -> pure ()
      SubmitFail x -> left . SplittingSubmissionError $ mconcat
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
    -> Fund
    -> Word64
    -> Word64
    -> Int
    -> TxOutValue era
    -> [(Tx era, [Fund])]
    -> [(Tx era, [Fund])]
  createSplittingTxs sKey initialFund numTxOuts maxOutsPerInitTx identityIndex txOut acc
    | numTxOuts <= 0 = reverse acc
    | otherwise =
        let numOutsPerInitTx = min maxOutsPerInitTx numTxOuts
            -- same TxOut for all
            outs = zip [identityIndex ..
                        identityIndex + fromIntegral numOutsPerInitTx - 1]
                       (repeat (TxOut globalOutAddr txOut))
            (mFunds, _fees, outIndices, splitTx) =
              mkTransactionGen sKey (initialFund :| []) globalOutAddr outs TxMetadataNone fee
            !splitTxId = getTxId $ getTxBody splitTx
            txIOList = flip map (Map.toList outIndices) $
                \(_, txInIndex) ->
                  let !txIn  = TxIn splitTxId txInIndex
                  in mkFund txIn txOut
        in
          case mFunds of
            Nothing                 -> reverse $ (splitTx, txIOList) : acc
            Just (txInIndex, val) ->
              let !txInChange  = TxIn splitTxId txInIndex
                  !txChangeValue = mkTxOutValueAdaOnly @ era val
              in
                -- from the change create the next tx with numOutsPerInitTx UTxO entries
                createSplittingTxs sKey
                                   (mkFund txInChange txChangeValue)
                                   (numTxOuts - numOutsPerInitTx)
                                   numOutsPerInitTx
                                   (identityIndex + fromIntegral numOutsPerInitTx)
                                   txOut
                                   ((splitTx, txIOList) : acc)

-----------------------------------------------------------------------------------------
-- | Run benchmark using top level tracers..
-----------------------------------------------------------------------------------------

-- This is the entry point for the CLI args tx-generator
{-# DEPRECATED runBenchmark "to be removed: use asyncBenchmark instead" #-}
runBenchmark :: forall era. IsShelleyBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> Tracer IO NodeToNodeSubmissionTrace
  -> ConnectClient
  -> NonEmpty NodeIPv4Address
  -> TPSRate
  -> SubmissionErrorPolicy
  -> [Tx era]
  -> ExceptT TxGenError IO ()
runBenchmark
  traceSubmit
  traceN2N
  connectClient
  targets
  tpsRate
  errorPolicy
  finalTransactions
  = do
    ctl <- asyncBenchmark
                       traceSubmit
                       traceN2N
                       connectClient
                       targets
                       tpsRate
                       errorPolicy
                       finalTransactions
    waitBenchmark traceSubmit ctl

type AsyncBenchmarkControl = (Async (), [Async ()], IO SubmissionSummary)

waitBenchmark :: Tracer IO (TraceBenchTxSubmit TxId) -> AsyncBenchmarkControl -> ExceptT TxGenError IO ()
waitBenchmark traceSubmit (feeder, workers, mkSummary) = liftIO $ do
  mapM_ wait (feeder : workers)
  traceWith traceSubmit =<< TraceBenchTxSubSummary <$> mkSummary

asyncBenchmark :: forall era. IsShelleyBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> Tracer IO NodeToNodeSubmissionTrace
  -> ConnectClient
  -> NonEmpty NodeIPv4Address
  -> TPSRate
  -> SubmissionErrorPolicy
  -> [Tx era]
  -> ExceptT TxGenError IO AsyncBenchmarkControl
asyncBenchmark
  traceSubmit
  traceN2N
  connectClient
  targets
  tpsRate
  errorPolicy
  finalTransactions
  = do
  let
    traceDebug :: String -> ExceptT TxGenError IO ()
    traceDebug =   liftIO . traceWith traceSubmit . TraceBenchTxSubDebug

  traceDebug "******* Tx generator, phase 2: pay to recipients *******"

  remoteAddresses <- forM targets $ \targetNodeAddress -> do
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

  let numTargets :: Natural = fromIntegral $ NE.length targets

  traceDebug $ "******* Tx generator, launching Tx peers:  " ++ show (NE.length remoteAddresses) ++ " of them"
  liftIO $ do
    submission :: Submission IO era  <- mkSubmission traceSubmit $
                    SubmissionParams
                    { spTps           = tpsRate
                    , spTargets       = numTargets
                    , spQueueLen      = 32
                    , spErrorPolicy   = errorPolicy
                    }
    allAsyncs <- forM (zip [0..] $ NE.toList remoteAddresses) $
      \(i, remoteAddr) ->
        launchTxPeer
              traceSubmit
              traceN2N
              connectClient
              remoteAddr
              submission
              i
    tpsFeeder <- async $ tpsLimitedTxFeeder submission finalTransactions
    return (tpsFeeder, allAsyncs, mkSubmissionSummary submission)

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
  .  IsShelleyBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> Lovelace
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> NumberOfOutputsPerTx
  -> TxAdditionalSize
  -> AddressInEra era
  -> SigningKey PaymentKey
  -> Int
  -> [Fund]
  -> ExceptT TxGenError IO [Tx era]
txGenerator
  tracer
  txFee
  (NumberOfTxs numOfTransactions)
  (NumberOfInputsPerTx numOfInsPerTx)
  (NumberOfOutputsPerTx numOfOutsPerTx)
  (TxAdditionalSize txAdditionalSize)
  recipientAddress
  sourceKey
  numOfTargetNodes
  fundsWithSufficientCoins
  = do
  liftIO . traceWith tracer . TraceBenchTxSubDebug
    $ " Generating " ++ show numOfTransactions
      ++ " transactions, for " ++ show numOfTargetNodes
      ++ " peers, fee " ++ show txFee
      ++ ", value " ++ show valueForRecipient
      ++ ", totalValue " ++ show totalValue
  metadata <- case mkMetadata txAdditionalSize of
    Right m -> return m
    Left err -> throwE $ BadPayloadSize $ pack err
  txs <- createMainTxs numOfTransactions numOfInsPerTx metadata fundsWithSufficientCoins
  liftIO . traceWith tracer . TraceBenchTxSubDebug
    $ " Done, " ++ show numOfTransactions ++ " were generated."
  pure txs
 where
  -- Num of recipients is equal to 'numOuts', so we think of
  -- recipients as the people we're going to pay to.
  recipients = zip [initRecipientIndex .. initRecipientIndex + numOfOutsPerTx - 1]
                   (repeat txOut)
  initRecipientIndex = 0 :: Int
  -- The same output for all transactions.
  valueForRecipient = quantityToLovelace $ Quantity 1000000 -- 10 ADA
  !txOut = TxOut recipientAddress (mkTxOutValueAdaOnly valueForRecipient)
  totalValue = valueForRecipient + txFee
  -- Send possible change to the same 'recipientAddress'.
  addressForChange = recipientAddress

  -- Create all main transactions, using available funds.
  createMainTxs
    :: Word64
    -> Int
    -> TxMetadataInEra era
    -> [Fund]
    -> ExceptT TxGenError IO [Tx era]
  createMainTxs 0 _ _ _= right []
  createMainTxs txsNum insNumPerTx metadata funds = do
    (txInputs, updatedFunds) <- getTxInputs insNumPerTx funds
    let (_, _, _, txAux :: Tx era) =
          mkTransactionGen
            sourceKey
            (NE.fromList txInputs)
            addressForChange
            recipients
            metadata
            txFee
    (txAux :) <$> createMainTxs (txsNum - 1) insNumPerTx metadata updatedFunds

  -- Get inputs for one main transaction, using available funds.
  getTxInputs
    :: Int
    -> [Fund]
    -> ExceptT TxGenError IO ( [Fund] , [Fund])
  getTxInputs 0 funds = right ([], funds)
  getTxInputs insNumPerTx funds = do
    (found, updatedFunds) <- findAvailableFunds funds totalValue
    (inputs, updatedFunds') <- getTxInputs (insNumPerTx - 1) updatedFunds
    right (found : inputs, updatedFunds')

  -- Find a source of available funds, removing it from the availableFunds
  -- for preventing of double spending.
  findAvailableFunds
    :: [Fund]     -- funds we are trying to find in
    -> Lovelace                -- with at least this associated value
    -> ExceptT TxGenError IO (Fund, [Fund])
  findAvailableFunds funds thresh =
    case break (predTxD thresh) funds of
      (_, [])    ->
        left $ InsufficientFundsForRecipientTx
                 thresh
                 (maximum $ map fundAdaValue funds)
      (toofews, found:rest) -> right (found, toofews <> rest)

  -- Find the first tx output that contains sufficient amount of money.
  predTxD :: Lovelace -> Fund -> Bool
  predTxD valueThreshold f = fundAdaValue f >= valueThreshold

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
  .  IsShelleyBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> Tracer IO NodeToNodeSubmissionTrace
  -> ConnectClient
  -> Network.Socket.AddrInfo
  -- Remote address
  -> Submission IO  era
  -- Mutable state shared between submission threads
  -> Natural
  -- Thread index
  -> IO (Async ())
launchTxPeer traceSubmit traceN2N connectClient remoteAddr sub tix =
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
           LogErrors   -> traceWith traceSubmit $
             TraceBenchTxSubError (pack errDesc))
     $ connectClient remoteAddr
        (txSubmissionClient traceN2N traceSubmit sub tix)
