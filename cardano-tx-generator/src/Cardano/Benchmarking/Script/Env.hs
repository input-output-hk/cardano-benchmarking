{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.Benchmarking.Script.Env
where

import           Prelude


import           Data.Functor.Identity
import           Data.Dependent.Sum (DSum(..) , (==>) )
import qualified Data.Dependent.Sum as DSum
import           Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import           Data.Constraint.Extras.TH (deriveArgDict)
--import           Data.GADT.Show
import           Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import           Data.GADT.Show.TH (deriveGShow)

import           Control.Monad.Trans.RWS.Strict

import           Cardano.Api

import           Cardano.Benchmarking.Types
  
data Tag v where
  TInitCooldown         :: Tag InitCooldown  
  TNumberOfInputsPerTx  :: Tag NumberOfInputsPerTx
  TNumberOfOutputsPerTx :: Tag NumberOfOutputsPerTx
  TNumberOfTxs          :: Tag NumberOfTxs
  TTPSRate              :: Tag TPSRate
  TFee                  :: Tag Lovelace
  TTTL                  :: Tag SlotNo
  TTxAdditionalSize     :: Tag TxAdditionalSize

deriveGEq ''Tag
deriveGCompare ''Tag
deriveGShow ''Tag
deriveArgDict ''Tag

type Env = DMap Tag Identity

emptyEnv :: Env
emptyEnv = DMap.empty

data Action where
  Set:: DSum Tag Identity -> Action

defaultSetup :: [Action]
defaultSetup = map Set [
    TInitCooldown         ==> InitCooldown 20
  , TNumberOfInputsPerTx  ==> NumberOfInputsPerTx 1
  , TNumberOfOutputsPerTx ==> NumberOfOutputsPerTx 1
  , TNumberOfTxs          ==> NumberOfTxs 100
  , TTPSRate              ==> TPSRate 10
  , TTxAdditionalSize     ==> TxAdditionalSize 0
  ]

type ActionM a = RWST () () Env IO a

action :: Action -> ActionM ()
action a = case a of
  Set (key :=> val) -> modify $ DMap.insert key val
