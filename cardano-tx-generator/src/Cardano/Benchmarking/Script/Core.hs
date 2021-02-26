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

import           Cardano.Api ( AddressInEra, CardanoEra(..), InAnyCardanoEra(..), IsShelleyBasedEra
                             )

import           Cardano.Benchmarking.GeneratorTx as Core (readSigningKey, secureGenesisFund, splitFunds, TxGenError)
import           Cardano.Benchmarking.Types as Core (NumberOfTxs(..), InitCooldown(..))
import           Cardano.Benchmarking.GeneratorTx.Tx as Core (keyAddress, Fund)
import           Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition as Core (startProtocol)
import           Cardano.Benchmarking.OuroborosImports as Core
                   (LocalSubmitTx, SigningKeyFile
                   , getGenesis, protocolToNetworkId, makeLocalConnectInfo, submitTxToNodeLocal)
import           Cardano.Benchmarking.Tracer as Core (createTracers, btTxSubmit_)

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
  socket    <- get $ User TLocalSocket
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
  fee      <- get $ User TFee
  ttl      <- get $ User TTTL
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
  fee      <- get $ User TFee
  address  <- getName destAddr
  fund     <- getName sourceFund
  key      <- getName sourceFundKey
  txIn     <- get $ User TNumberOfInputsPerTx
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

delay :: ActionM ()
delay = do
  (InitCooldown t) <- get $ User TInitCooldown
  liftIO $ threadDelay $ 1000000 * t
