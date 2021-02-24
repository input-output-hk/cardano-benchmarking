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

import           Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition as Core (startProtocol)
import           Cardano.Benchmarking.OuroborosImports as Core (getGenesis)
import           Cardano.Benchmarking.Tracer as Core (createTracers)

import           Cardano.Benchmarking.Script.Env
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
