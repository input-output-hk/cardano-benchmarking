{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Core
where

import           Prelude
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import           Cardano.Benchmarking.GeneratorTx as Core (readSigningKey)
import           Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition as Core (startProtocol)
import           Cardano.Benchmarking.OuroborosImports as Core
                   (LocalSubmitTx, SigningKeyFile
                   , getGenesis, protocolToNetworkId, makeLocalConnectInfo, submitTxToNodeLocal)
import           Cardano.Benchmarking.Tracer as Core (createTracers)

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
    Left err -> lift $ throwE $ TxGenError err
    Right key -> set (NamedKey name) key

getLocalSubmitTx :: ActionM LocalSubmitTx
getLocalSubmitTx = do
  networkId <- get NetworkId
  socket    <- get $ User TLocalSocket
  return $ submitTxToNodeLocal $ makeLocalConnectInfo networkId socket


{-
type SecureGenesisFund era =
     Fee
  -> TTL
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> ScriptM Fund

    secureGenesisFund :: IsShelleyBasedEra era => SecureGenesisFund era
    secureGenesisFund = GeneratorTx.secureGenesisFund
                (btTxSubmit_ tracers)
                (submitTxToNodeLocal localConnectInfo)
                networkId
                (getGenesis ptcl)


-}
