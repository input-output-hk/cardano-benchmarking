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

import           Data.Dependent.Sum (DSum(..) , (==>) )
import qualified Data.Dependent.Map as DMap

import           Control.Monad.Trans.RWS.Strict

import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters as Setters
import           Cardano.Benchmarking.Script.Store

data Action where
  Set              :: SetKeyVal -> Action
  StartProtocol    :: FilePath -> Action
  InitLocalConnect :: FilePath -> Action
  deriving (Show)

defaultSetup :: [Action]
defaultSetup = map Set [
    TInitCooldown         ==> InitCooldown 20
  , TNumberOfInputsPerTx  ==> NumberOfInputsPerTx 1
  , TNumberOfOutputsPerTx ==> NumberOfOutputsPerTx 1
  , TNumberOfTxs          ==> NumberOfTxs 100
  , TTPSRate              ==> TPSRate 10
  , TTxAdditionalSize     ==> TxAdditionalSize 0
  ]

action :: Action -> ActionM ()
action a = case a of
  Set (key :=> val) -> modify $ DMap.insert (User key) val
