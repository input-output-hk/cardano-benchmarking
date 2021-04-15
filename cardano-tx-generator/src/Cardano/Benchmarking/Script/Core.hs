{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use uncurry" -}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-} --
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Core
where

import           Prelude
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Control.Concurrent (threadDelay)

import           Cardano.Api ( AsType(..), CardanoEra(..), InAnyCardanoEra(..), AnyCardanoEra(..), IsShelleyBasedEra, Tx
                             , NetworkId(..), cardanoEra
                             , CardanoMode, LocalNodeConnectInfo
                             , getLocalChainTip, queryNodeLocalState, QueryInMode( QueryCurrentEra), ConsensusModeIsMultiEra( CardanoModeIsMultiEra )
                             , chainTipToChainPoint )
import           Cardano.Benchmarking.GeneratorTx as Core
                   (AsyncBenchmarkControl, asyncBenchmark, waitBenchmark, readSigningKey, secureGenesisFund, splitFunds, txGenerator, TxGenError)
import           Cardano.Benchmarking.Types as Core (NumberOfTxs(..), InitCooldown(..), SubmissionErrorPolicy(..))
import           Cardano.Benchmarking.GeneratorTx.Tx as Core (keyAddress)
import           Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition as Core (startProtocol)
import           Cardano.Benchmarking.GeneratorTx.NodeToNode (ConnectClient, benchmarkConnectTxSubmit)
import           Cardano.Benchmarking.OuroborosImports as Core
                   (LocalSubmitTx, SigningKeyFile
                   , getGenesis, protocolToNetworkId, protocolToCodecConfig, makeLocalConnectInfo, submitTxToNodeLocal)
import           Cardano.Benchmarking.Tracer as Core (createTracers, btTxSubmit_, btN2N_, btConnect_, btSubmission_)

import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store as Store

liftCoreWithEra :: (forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO x) -> ActionM (Either TxGenError x)
liftCoreWithEra coreCall = withEra ( liftIO . runExceptT . coreCall)

withEra :: (forall era. IsShelleyBasedEra era => AsType era -> ActionM x) -> ActionM x
withEra action = do
  era <- get $ User TEra
  case era of
    AnyCardanoEra MaryEra    -> action AsMaryEra
    AnyCardanoEra AllegraEra -> action AsAllegraEra
    AnyCardanoEra ShelleyEra -> action AsShelleyEra
    AnyCardanoEra ByronEra   -> error "byron not supported"

startProtocol :: FilePath -> ActionM ()
startProtocol filePath = do
  liftIO (runExceptT $ Core.startProtocol filePath) >>= \case
    Left err -> throwE $ CliError err
    Right (loggingLayer, protocol) -> do
      set LoggingLayer loggingLayer
      set Protocol protocol
      set BenchTracers $ Core.createTracers loggingLayer
      set Genesis $ Core.getGenesis protocol
      set NetworkId $ protocolToNetworkId protocol

readSigningKey :: KeyName -> SigningKeyFile -> ActionM ()
readSigningKey name filePath =
  liftIO ( runExceptT $ Core.readSigningKey filePath) >>= \case
    Left err -> liftTxGenError err
    Right key -> setName name key

getLocalSubmitTx :: ActionM LocalSubmitTx
getLocalSubmitTx = submitTxToNodeLocal <$> getLocalConnectInfo

secureGenesisFund
   :: FundName
   -> KeyName
   -> KeyName
   -> ActionM ()
secureGenesisFund fundName destKey genesisKeyName = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmitTx <- getLocalSubmitTx
  networkId <- get NetworkId
  genesis  <- get Genesis
  fee      <- getUser TFee
  ttl      <- getUser TTTL
  fundKey  <- getName destKey
  genesisKey  <- getName genesisKeyName
  let
    coreCall :: forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO Store.Fund
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId fundKey
      f <- Core.secureGenesisFund tracer localSubmitTx networkId genesis fee ttl genesisKey addr
      return (f, fundKey)
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right fund -> setName fundName fund

splitFundN
   :: NumberOfTxs
   -> KeyName
   -> FundName
   -> ActionM [Store.Fund]
splitFundN count destKeyName sourceFund = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmitTx <- getLocalSubmitTx
  networkId <- get NetworkId
  fee      <- getUser TFee
  destKey  <- getName destKeyName
  (fund, fundKey) <- consumeName sourceFund
  txIn     <- getUser TNumberOfInputsPerTx
  let
    coreCall :: forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO [Store.Fund]
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId fundKey
      f <- Core.splitFunds tracer localSubmitTx fee count txIn fundKey addr fund
      return $ zip f $ repeat destKey
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right funds -> return funds

splitFund
   :: [FundName]
   -> KeyName
   -> FundName
   -> ActionM ()
splitFund newFunds destKey sourceFund = do
  funds <- splitFundN (NumberOfTxs $ fromIntegral $ length newFunds) destKey sourceFund
  forM_ (zip newFunds funds) $ \(name, f) -> setName name f

splitFundToList
   :: FundListName
   -> KeyName
   -> FundName
   -> ActionM ()
splitFundToList newFunds destKey sourceFund = do
  count <- getUser TNumberOfTxs
  funds <- splitFundN count destKey sourceFund
  setName newFunds funds

delay :: ActionM ()
delay = do
  (InitCooldown t) <- getUser TInitCooldown
  liftIO $ threadDelay $ 1000000 * t

prepareTxList
   :: TxListName
   -> KeyName
   -> FundListName
   -> ActionM ()
prepareTxList name destKey srcFundName = do
  tracer   <- btTxSubmit_ <$> get BenchTracers
  networkId <- get NetworkId
  fee      <- getUser TFee
  fundList <- consumeName srcFundName
  key      <- getName destKey
  txIn     <- getUser TNumberOfInputsPerTx
  txOut    <- getUser TNumberOfOutputsPerTx
  count    <- getUser TNumberOfTxs
  payload  <- getUser TTxAdditionalSize
  let
    coreCall :: forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO (InAnyCardanoEra TxList)
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId key
      ----------------------------------------------------TODO : Constant 1 ???
      l <- Core.txGenerator tracer fee count txIn txOut payload addr (snd $ head fundList) 1 (map fst fundList)
      return $ InAnyCardanoEra cardanoEra $ TxList l
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right l -> setName name l

waitBenchmarkCore :: AsyncBenchmarkControl ->  ActionM ()
waitBenchmarkCore ctl = do
  tracers  <- get BenchTracers
  _ <- liftIO $ runExceptT $ Core.waitBenchmark (btTxSubmit_ tracers) ctl
  return ()

asyncBenchmarkCore :: ThreadName -> TxListName -> ActionM AsyncBenchmarkControl
asyncBenchmarkCore (ThreadName threadName) transactions = do
  tracers  <- get BenchTracers
  targets  <- getUser TTargets
  tps      <- getUser TTPSRate
  txs      <- getName transactions
  (Testnet networkMagic) <- get NetworkId
  protocol <- get Protocol
  ioManager <- askIOManager
  let
    connectClient :: ConnectClient
    connectClient  = benchmarkConnectTxSubmit
                       ioManager
                       (btConnect_ tracers)
                       (btSubmission_ tracers)
                       (protocolToCodecConfig protocol)
                       networkMagic

    coreCall :: forall era. IsShelleyBasedEra era => [Tx era] -> ExceptT TxGenError IO AsyncBenchmarkControl
    coreCall l = Core.asyncBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient threadName targets tps LogErrors l
  ret <- liftIO $ runExceptT $ case txs of
    InAnyCardanoEra MaryEra    (TxList l) -> coreCall l
    InAnyCardanoEra AllegraEra (TxList l) -> coreCall l
    InAnyCardanoEra ShelleyEra (TxList l) -> coreCall l
    InAnyCardanoEra ByronEra   _ -> error "byron not supported"
  case ret of
    Left err -> liftTxGenError err
    Right ctl -> return ctl


--{-# DEPRECATED runBenchmark "to be removed: use asynBenchmark" #-}
runBenchmark :: TxListName -> ActionM ()
runBenchmark transactions = asyncBenchmarkCore (ThreadName "UnlabeledThread") transactions >>= waitBenchmarkCore

asyncBenchmark :: ThreadName -> TxListName -> ActionM ()
asyncBenchmark controlName txList = asyncBenchmarkCore controlName txList >>= setName controlName

waitBenchmark :: ThreadName -> ActionM ()
waitBenchmark n = getName n >>= waitBenchmarkCore

cancelBenchmark :: ThreadName -> ActionM ()
cancelBenchmark n = do
  ctl@(_, _ , _ , shutdownAction) <- getName n
  liftIO shutdownAction
  waitBenchmarkCore ctl

getLocalConnectInfo :: ActionM  (LocalNodeConnectInfo CardanoMode)
getLocalConnectInfo = makeLocalConnectInfo <$> get NetworkId <*> getUser TLocalSocket

queryEra :: ActionM AnyCardanoEra
queryEra = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  ret <- liftIO $ queryNodeLocalState localNodeConnectInfo (Just $ chainTipToChainPoint chainTip) $ QueryCurrentEra CardanoModeIsMultiEra
  case ret of
    Right era -> return era
    Left err -> throwE $ ApiError $ show err

waitForEra :: AnyCardanoEra -> ActionM ()
waitForEra era = do
  currentEra <- queryEra
  if currentEra == era
    then return ()
    else do
      traceError $ "Current era: " ++ show currentEra ++ " Waiting for: " ++ show era
      liftIO $ threadDelay 1_000_000
      waitForEra era
{-
This is for dirty hacking and testing and quick-fixes.
Its a function that can be called from the JSON scripts
and for which the JSON encoding is "reserved".
-}
reserved :: [String] -> ActionM ()
reserved _ = do
  throwE $ UserError "no dirty hack is implemented"
