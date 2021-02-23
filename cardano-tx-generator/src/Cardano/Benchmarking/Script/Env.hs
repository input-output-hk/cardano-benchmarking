{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
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

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.RWS.Strict

import           Cardano.Benchmarking.Script.Setters as Setters
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.GeneratorTx.Error (TxGenError)

type Env = DMap Store Identity

emptyEnv :: Env
emptyEnv = DMap.empty

type SetKeyVal = DSum Setters.Tag Identity

data Error where
  LookupError :: Error
  TxGenError  :: TxGenError -> Error
  deriving (Show)

type ActionM a = RWST () () Env (ExceptT Error IO) a
