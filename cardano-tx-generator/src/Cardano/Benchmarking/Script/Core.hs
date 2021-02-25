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
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import           Cardano.Api ( AddressInEra, CardanoEra(..), InAnyCardanoEra(..), IsShelleyBasedEra
                             )

import           Cardano.Benchmarking.GeneratorTx as Core (readSigningKey, secureGenesisFund, TxGenError)
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

readSigningKey :: Name -> SigningKeyFile -> ActionM ()
readSigningKey name filePath =
  (liftIO $ runExceptT $ Core.readSigningKey filePath) >>= \case
    Left err -> liftTxGenError err
    Right key -> set (NamedKey name) key

keyAddress :: forall era. IsShelleyBasedEra era => Name -> Name -> CardanoEra era -> ActionM ()
keyAddress addrName keyName era = do
  key <- get $ NamedKey keyName
  networkId <- get NetworkId
  let addr = Core.keyAddress @ era networkId key
  set (NamedAddress addrName) (InAnyCardanoEra era addr)

getLocalSubmitTx :: ActionM LocalSubmitTx
getLocalSubmitTx = do
  networkId <- get NetworkId
  socket    <- get $ User TLocalSocket
  return $ submitTxToNodeLocal $ makeLocalConnectInfo networkId socket

secureGenesisFund ::
      Name
   -> Name
   -> Name
   -> ActionM ()
secureGenesisFund fundName fundAddr genesisKey = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmitTx <- getLocalSubmitTx
  networkId <- get NetworkId
  genesis  <- get Genesis
  fee      <- get $ User TFee
  ttl      <- get $ User TTTL
  addr <- get $ NamedAddress fundAddr
  key  <- get $ NamedKey genesisKey
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
    Right fund -> set (NamedFund fundName) fund
