{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Core
where

import           Prelude
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Concurrent (threadDelay)

import           Cardano.Api ( AddressInEra, CardanoEra(..), InAnyCardanoEra(..), IsShelleyBasedEra, Tx
                             , NetworkId(..), cardanoEra)

import           Cardano.Benchmarking.GeneratorTx as Core
                   (readSigningKey, runBenchmark, secureGenesisFund, splitFunds, txGenerator, TxGenError)
import           Cardano.Benchmarking.Types as Core (NumberOfTxs(..), InitCooldown(..), SubmissionErrorPolicy(..))
import           Cardano.Benchmarking.GeneratorTx.Tx as Core (keyAddress, Fund)
import           Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition as Core (startProtocol)
import           Cardano.Benchmarking.GeneratorTx.NodeToNode (ConnectClient, benchmarkConnectTxSubmit)
import           Cardano.Benchmarking.OuroborosImports as Core
                   (LocalSubmitTx, SigningKeyFile
                   , getGenesis, protocolToNetworkId, protocolToCodecConfig, makeLocalConnectInfo, submitTxToNodeLocal)
import           Cardano.Benchmarking.Tracer as Core (createTracers, btTxSubmit_, btN2N_, btConnect_, btSubmission_)

import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store

startProtocol :: FilePath -> ActionM ()
startProtocol filePath = do
  (liftIO $ runExceptT $ Core.startProtocol filePath) >>= \case
    Left err -> lift $ throwE $ CliError err     
    Right (loggingLayer, protocol) -> do
      set LoggingLayer loggingLayer
      set Protocol protocol
      set BenchTracers $ Core.createTracers loggingLayer
      set Genesis $ Core.getGenesis protocol
      set NetworkId $ protocolToNetworkId protocol

readSigningKey :: KeyName -> SigningKeyFile -> ActionM ()
readSigningKey name filePath =
  (liftIO $ runExceptT $ Core.readSigningKey filePath) >>= \case
    Left err -> liftTxGenError err
    Right key -> setName name key

keyAddress :: forall era. IsShelleyBasedEra era => AddressName -> KeyName -> CardanoEra era -> ActionM ()
keyAddress addrName keyName era = do
  key <- getName keyName
  networkId <- get NetworkId
  let addr = Core.keyAddress @ era networkId key
  setName addrName (InAnyCardanoEra era addr)

getLocalSubmitTx :: ActionM LocalSubmitTx
getLocalSubmitTx = do
  networkId <- get NetworkId
  socket    <- getUser TLocalSocket
  return $ submitTxToNodeLocal $ makeLocalConnectInfo networkId socket

secureGenesisFund ::
      FundName
   -> AddressName
   -> KeyName
   -> ActionM ()
secureGenesisFund fundName fundAddr genesisKey = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmitTx <- getLocalSubmitTx
  networkId <- get NetworkId
  genesis  <- get Genesis
  fee      <- getUser TFee
  ttl      <- getUser TTTL
  addr <- getName fundAddr
  key  <- getName genesisKey
  let
    coreCall :: forall era. IsShelleyBasedEra era => AddressInEra era -> ExceptT TxGenError IO Fund
    coreCall = Core.secureGenesisFund tracer localSubmitTx networkId genesis fee ttl key
  ret <- liftIO $ runExceptT $ case addr of
    InAnyCardanoEra MaryEra    a -> coreCall a
    InAnyCardanoEra AllegraEra a -> coreCall a
    InAnyCardanoEra ShelleyEra a -> coreCall a
    InAnyCardanoEra ByronEra   _ -> error "byron not supported"
  case ret of
    Left err -> liftTxGenError err
    Right fund -> setName fundName fund

splitFundN ::
      NumberOfTxs
   -> AddressName
   -> FundName
   -> KeyName
   -> ActionM [Fund]
splitFundN count destAddr sourceFund sourceFundKey = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmitTx <- getLocalSubmitTx
  fee      <- getUser TFee
  address  <- getName destAddr
  fund     <- getName sourceFund
  key      <- getName sourceFundKey
  txIn     <- getUser TNumberOfInputsPerTx
  let
    coreCall :: forall era. IsShelleyBasedEra era => AddressInEra era -> ExceptT TxGenError IO [Fund]
    coreCall addr = Core.splitFunds tracer localSubmitTx fee count txIn key addr fund
  ret <- liftIO $ runExceptT $ case address of
    InAnyCardanoEra MaryEra    a -> coreCall a
    InAnyCardanoEra AllegraEra a -> coreCall a
    InAnyCardanoEra ShelleyEra a -> coreCall a
    InAnyCardanoEra ByronEra   _ -> error "byron not supported"
  case ret of
    Left err -> liftTxGenError err
    Right funds -> return funds

splitFund ::
      [FundName]
   -> AddressName
   -> FundName
   -> KeyName
   -> ActionM ()
splitFund newFunds destAddr sourceFund sourceFundKey = do
  funds <- splitFundN (NumberOfTxs $ fromIntegral $ length newFunds) destAddr sourceFund sourceFundKey
  forM_ (zip newFunds funds) $ \(name, f) -> setName name f

splitFundToList ::
      FundListName
   -> AddressName
   -> FundName
   -> KeyName
   -> ActionM ()
splitFundToList newFunds destAddr sourceFund sourceFundKey = do
  count <- getUser TNumberOfTxs
  funds <- splitFundN count destAddr sourceFund sourceFundKey
  setName newFunds funds

delay :: ActionM ()
delay = do
  (InitCooldown t) <- getUser TInitCooldown
  liftIO $ threadDelay $ 1000000 * t

prepareTxList ::
      TxListName
   -> AddressName
   -> FundListName
   -> KeyName
   -> ActionM ()
prepareTxList name destAddr srcFund fundKey = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  fee      <- getUser TFee
  address  <- getName destAddr
  fund     <- getName srcFund
  key      <- getName fundKey
  txIn     <- getUser TNumberOfInputsPerTx
  txOut    <- getUser TNumberOfOutputsPerTx
  count    <- getUser TNumberOfTxs
  payload  <- getUser TTxAdditionalSize
  let
    coreCall :: forall era. IsShelleyBasedEra era => AddressInEra era -> ExceptT TxGenError IO (InAnyCardanoEra TxList)
    coreCall addr = do
      ----------------------------------------------------TODO : constant 1
      l <- Core.txGenerator tracer fee count txIn txOut payload addr key 1 fund
      return $ InAnyCardanoEra cardanoEra $ TxList l
  ret <- liftIO $ runExceptT $ case address of
    InAnyCardanoEra MaryEra    a -> coreCall a
    InAnyCardanoEra AllegraEra a -> coreCall a
    InAnyCardanoEra ShelleyEra a -> coreCall a
    InAnyCardanoEra ByronEra   _ -> error "byron not supported"
  case ret of
    Left err -> liftTxGenError err
    Right l -> setName name l


runBenchmark ::
      TxListName
   -> ActionM ()
runBenchmark transactions = do
  tracers  <- get BenchTracers
  let
    tr1 =  btTxSubmit_ tracers
    tr2 =  btN2N_ tracers
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

    coreCall :: forall era. IsShelleyBasedEra era => [Tx era] -> ExceptT TxGenError IO ()
    coreCall l = Core.runBenchmark tr1 tr2 connectClient targets tps LogErrors l
  ret <- liftIO $ runExceptT $ case txs of
    InAnyCardanoEra MaryEra    (TxList l) -> coreCall l
    InAnyCardanoEra AllegraEra (TxList l) -> coreCall l
    InAnyCardanoEra ShelleyEra (TxList l) -> coreCall l
    InAnyCardanoEra ByronEra   _ -> error "byron not supported"
  case ret of
    Left err -> liftTxGenError err
    Right () -> return ()
