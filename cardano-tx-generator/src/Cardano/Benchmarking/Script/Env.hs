{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Env
where

import           Prelude

import           Data.Functor.Identity
import           Data.Dependent.Sum (DSum(..))
import           Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.RWS.Strict (RWST)
import qualified Control.Monad.Trans.RWS.Strict as RWS

import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.Benchmarking.Script.Setters as Setters
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.GeneratorTx.Error (TxGenError)
import           Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition (CliError)

type Env = DMap Store Identity

emptyEnv :: Env
emptyEnv = DMap.empty

type SetKeyVal = DSum Setters.Tag Identity

data Error where
  LookupError :: Store v    -> Error
  TxGenError  :: TxGenError -> Error
  CliError    :: CliError   -> Error

liftTxGenError :: TxGenError -> ActionM a
liftTxGenError = lift . throwE . TxGenError

deriving instance Show Error

type ActionM a = RWST IOManager () Env (ExceptT Error IO) a

askIOManager :: ActionM IOManager
askIOManager = RWS.ask

set :: Store v -> v -> ActionM ()
set key val = RWS.modify $ DMap.insert key (pure val)

unSet :: Store v -> ActionM ()
unSet key = RWS.modify $ DMap.delete key

setName :: Name v -> v -> ActionM ()
setName = set . Named

get :: Store v -> ActionM v
get key = do
  (RWS.gets $ DMap.lookup key) >>= \case
    Just (Identity v) -> return v
    Nothing -> lift $ throwE $ LookupError key

getName :: Name v -> ActionM v
getName = get . Named

getUser :: Tag v -> ActionM v
getUser = get . User

consumeName :: Name v -> ActionM v
consumeName n = do
  v <- getName n
  unSet $ Named n
  return v
