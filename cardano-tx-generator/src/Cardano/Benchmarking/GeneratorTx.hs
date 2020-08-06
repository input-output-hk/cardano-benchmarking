{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , genesisBenchmarkRunner
  ) where

import           Cardano.Prelude
import           Prelude (String, id)

import           Control.Concurrent (threadDelay)
import           Control.Monad (forM, forM_)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left, newExceptT, right)
import           Control.Tracer (traceWith)

import           Data.Foldable (find)
import qualified Data.IP as IP
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Maybe (Maybe (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Word (Word64)
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Family (..), SocketType (Stream),
                                 addrFamily, addrFlags, addrSocketType, defaultHints, getAddrInfo)

import           Cardano.Config.Types (NodeAddress (..), NodeHostAddress (..), SigningKeyFile (..))

import           Cardano.Api.TxSubmit
import           Cardano.Api.Typed

import           Cardano.Benchmarking.GeneratorTx.Benchmark
import           Cardano.Benchmarking.GeneratorTx.Era
import           Cardano.Benchmarking.GeneratorTx.Error
import           Cardano.Benchmarking.GeneratorTx.Genesis
import           Cardano.Benchmarking.GeneratorTx.NodeToNode
import           Cardano.Benchmarking.GeneratorTx.Submission
import           Cardano.Benchmarking.GeneratorTx.Tx


-----------------------------------------------------------------------------------------
-- | Genesis benchmark runner (we call it in 'Run.runNode').
--
--   Using a _richman_ (from genesis block) to supply some initial
--   amount of funds for disbursment.
-----------------------------------------------------------------------------------------
genesisBenchmarkRunner
  :: EraSupportsTxGen era
  => Benchmark -> Era era -> SigningKeyFile
  -> ExceptT TxGenError IO ()
genesisBenchmarkRunner b p keyFile = do
  key <- readSigningKey p keyFile
  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator, signing keys are ready *******"

  fundsWithGenesisMoney <- liftIO $
    prepareInitialFunds b p
      (extractGenesisFunds p key) key
      (keyAddress p key)

  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator, initial funds are prepared (sent to sourceAddress) *******"

  runBenchmark b p key (keyAddress p key) fundsWithGenesisMoney

{-------------------------------------------------------------------------------
  Main logic
-------------------------------------------------------------------------------}
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

-----------------------------------------------------------------------------------------
-- Obtain initial funds.
-----------------------------------------------------------------------------------------
prepareInitialFunds
  :: forall era
  .  EraSupportsTxGen era
  => Benchmark
  -> Era era
  -> (TxIn, TxOut era)
  -> SigningKeyOf era
  -> Address era
  -> IO (TxIn, TxOut era)
prepareInitialFunds
  Benchmark{bTxFee, bInitialTTL} p (_, TxOut _ genesisCoin) key toAddr = do
    r <- submitTx (eraLocalConnInfo p) (castTxMode tx)
    liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
      $ mconcat
      [ "******* Genesis funds move (", show txin, " -> ", show txout
      , ") submission result: "
      , show r]
    pure (txin, txout)
 where
   (tx, txin, txout) =
     genesisExpenditure p key toAddr genesisCoin bTxFee bInitialTTL

splitFunds
  :: forall era
  .  EraSupportsTxGen era
  => Benchmark
  -> Era era
  -> SigningKeyOf era
  -> (TxIn, TxOut era)
  -> ExceptT TxGenError IO (Set (TxIn, TxOut era))
splitFunds
    Benchmark{ bTxFee=fee@(Lovelace feeRaw), bTxCount=NumberOfTxs numTxs
             , bTxFanIn=NumberOfInputsPerTx txFanin
             }
    p sourceKey fundsTxIO@(_, (TxOut addr (Lovelace rawCoin))) = do
  let -- The number of splitting txout entries (corresponds to the number of all inputs we will need).
      numRequiredTxOuts = numTxs * fromIntegral txFanin
      splitFanout = 60 :: Word64 -- near the upper bound so as not to exceed the tx size limit
      (nFullTxs, remainder) = numRequiredTxOuts `divMod` splitFanout
      numSplitTxs = nFullTxs + if remainder > 0 then 1 else 0

  let -- Split the funds to 'numRequiredTxOuts' equal parts, subtracting the possible fees.
      -- a safe number for fees is numRequiredTxOuts' * feePerTx.
      splitValue = Lovelace $
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
    liftIO (submitTx (eraLocalConnInfo p) (castTxMode tx))
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
  right $ reCreateAvailableFunds splittingTxs
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
            outs = Set.fromList $
              zip [identityIndex ..
                   identityIndex + fromIntegral numOutsPerInitTx - 1]
                  (repeat txOut)
            (mFunds, _fees, outIndices, splitTx) =
              mkTransactionGen p sKey (txIO :| []) Nothing outs 0 fee
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
                  !txOutChange = TxOut srcAddr value
              in
                -- from the change create the next tx with numOutsPerInitTx UTxO entries
                createSplittingTxs sKey
                                   (txInChange, txOutChange)
                                   (numTxOuts - numOutsPerInitTx)
                                   numOutsPerInitTx
                                   (identityIndex + fromIntegral numOutsPerInitTx)
                                   txOut
                                   ((splitTx, txIOList) : acc)
  reCreateAvailableFunds
    :: [(Tx era, [(TxIn, TxOut era)])]
    -> Set (TxIn, TxOut era)
  reCreateAvailableFunds =
    Set.fromList . concatMap snd

-----------------------------------------------------------------------------------------
-- | Run benchmark using top level tracers..
-----------------------------------------------------------------------------------------

-- | Please note that there's a difference between Cardano tx and fiscal tx:
--   1. Cardano tx is a transaction from Cardano blockchain's point of view.
--   2. Fiscal tx is a transaction from recipient's point of view.
--   So if one Cardano tx contains 10 outputs (with addresses of 10 recipients),
--   we have 1 Cardano tx and 10 fiscal txs.
runBenchmark
  :: forall era
  .  (EraSupportsTxGen era)
  => Benchmark
  -> Era era
  -> SigningKeyOf era
  -> Address era
  -> (TxIn, TxOut era)
  -> ExceptT TxGenError IO ()
runBenchmark b@Benchmark{ bTargets
                        , bTps
                        , bInitCooldown=InitCooldown initCooldown
                        }
             p
             sourceKey
             recipientAddress
             fundsWithGenesisMoney = do
  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator, phase 1: make enough available UTxO entries using: " <> (show fundsWithGenesisMoney :: String)
  fundsWithSufficientCoins <-
    splitFunds b p sourceKey fundsWithGenesisMoney

  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator: waiting " ++ show initCooldown ++ "s *******"
  liftIO $ threadDelay (initCooldown*1000*1000)

  liftIO . traceWith (trTxSubmit p) . TraceBenchTxSubDebug
    $ "******* Tx generator, phase 2: pay to recipients *******"

  let localAddr :: Maybe Network.Socket.AddrInfo
      localAddr = Nothing

  remoteAddresses <- forM bTargets $ \targetNodeAddress -> do
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
  let numTargets :: Natural = fromIntegral $ NE.length bTargets
  txs :: [Tx era] <-
           txGenerator
              b p
              recipientAddress
              sourceKey
              (NE.length bTargets)
              fundsWithSufficientCoins

  liftIO $ do
    traceWith (trTxSubmit p) . TraceBenchTxSubDebug
        $ "******* Tx generator, launching Tx peers:  " ++ show (NE.length remoteAddresses) ++ " of them"
    submission <- mkSubmission (trTxSubmit p) $
                    SubmissionParams
                    { spTps      = bTps
                    , spTargets  = numTargets
                    , spQueueLen = 32
                    }
    allAsyncs <- forM (zip [0..] $ NE.toList remoteAddresses) $
      \(i, remoteAddr) ->
        launchTxPeer
              p
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

-----------------------------------------------------------------------------------------
-- | Work with tx generator thread (for Phase 2).
-----------------------------------------------------------------------------------------
txGenerator
  :: forall era
  .  EraSupportsTxGen era
  => Benchmark
  -> Era era
  -> Address era
  -> SigningKeyOf era
  -> Int
  -> Set (TxIn, TxOut era)
  -> ExceptT TxGenError IO [Tx era]
txGenerator Benchmark
            { bTxFee
            , bTxCount=NumberOfTxs numOfTransactions
            , bTxFanIn=NumberOfInputsPerTx numOfInsPerTx
            , bTxFanOut=NumberOfOutputsPerTx numOfOutsPerTx
            , bTxExtraPayload=txAdditionalSize
            }
            p recipientAddress sourceKey numOfTargetNodes
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
  valueForRecipient = Lovelace 100000000 -- 100 ADA, discuss this value.
  !txOut = TxOut recipientAddress valueForRecipient
  totalValue = valueForRecipient + bTxFee
  -- Send possible change to the same 'recipientAddress'.
  addressForChange = recipientAddress

  -- Create all main transactions, using available funds.
  createMainTxs
    :: Word64
    -> Int
    -> Set (TxIn, TxOut era)
    -> ExceptT TxGenError IO [Tx era]
  createMainTxs 0 _ _ = right []
  createMainTxs txsNum insNumPerTx funds = do
    (txInputs, updatedFunds) <- getTxInputs insNumPerTx funds
    let (_, _, _, txAux :: Tx era) =
          mkTransactionGen
            p
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
    -> Set (TxIn, TxOut era)
    -> ExceptT TxGenError IO ( [(TxIn, TxOut era)]
                             , Set (TxIn, TxOut era)
                             )
  getTxInputs 0 funds = right ([], funds)
  getTxInputs insNumPerTx funds = do
    (found, updatedFunds) <- findAvailableFunds funds totalValue
    (inputs, updatedFunds') <- getTxInputs (insNumPerTx - 1) updatedFunds
    right (found : inputs, updatedFunds')

  -- Find a source of available funds, removing it from the availableFunds
  -- for preventing of double spending.
  findAvailableFunds
    :: Set (TxIn, TxOut era)     -- funds we are trying to find in
    -> Lovelace                -- with at least this associated value
    -> ExceptT TxGenError IO ((TxIn, TxOut era), Set (TxIn, TxOut era))
  findAvailableFunds funds thresh =
    case find (predTxD thresh) funds of
      Nothing    -> left InsufficientFundsForRecipientTx
      Just found -> right (found, Set.delete found funds)

  -- Find the first tx output that contains sufficient amount of money.
  predTxD :: Lovelace -> (TxIn, TxOut era) -> Bool
  predTxD valueThreshold (_, TxOut _ coin) = coin >= valueThreshold

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
  .  EraSupportsTxGen era
  => Era era
  -> Maybe Network.Socket.AddrInfo
  -- local address binding (if wanted)
  -> Network.Socket.AddrInfo
  -- Remote address
  -> Submission IO era
  -- Mutable state shared between submission threads
  -> Natural
  -- Thread index
  -> IO (Async ())
launchTxPeer p localAddr remoteAddr ss ix =
  async $
    benchmarkConnectTxSubmit p localAddr remoteAddr
      (txSubmissionClient p (trN2N p) (trTxSubmit p) ss ix)
