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

module Cardano.Benchmarking.Script.Action
where

import           Prelude

import           Data.Functor.Identity
import           Data.Dependent.Sum (DSum(..) , (==>) )
import qualified Data.Dependent.Map as DMap

import           Control.Monad.Trans.RWS.Strict

import           Cardano.Benchmarking.OuroborosImports (SigningKeyFile)

import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters as Setters
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Script.Core

data Action where
  Set                :: SetKeyVal   -> Action
  StartProtocol      :: FilePath    -> Action
  ReadSigningKey     :: Name -> SigningKeyFile -> Action
  deriving (Show)

action :: Action -> ActionM ()
action a = case a of
  Set (key :=> (Identity val)) -> set (User key) val
  StartProtocol filePath -> startProtocol filePath
  ReadSigningKey name filePath -> readSigningKey name filePath
