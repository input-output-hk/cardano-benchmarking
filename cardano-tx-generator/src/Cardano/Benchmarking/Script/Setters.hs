{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Benchmarking.Script.Setters
where

import           Prelude

import GHC.Generics

import           Data.Dependent.Sum (DSum(..) , (==>) )

import           Data.Constraint.Extras.TH (deriveArgDict)
import           Data.GADT.Compare.TH (deriveGCompare, deriveGEq)

import           Data.GADT.Show.TH (deriveGShow)
import           Cardano.Api (Lovelace, SlotNo)

import           Cardano.Benchmarking.Types

-- Some boiler plate; ToDo may generate this.
data Tag v where
  TInitCooldown         :: Tag InitCooldown  
  TNumberOfInputsPerTx  :: Tag NumberOfInputsPerTx
  TNumberOfOutputsPerTx :: Tag NumberOfOutputsPerTx
  TNumberOfTxs          :: Tag NumberOfTxs
  TTPSRate              :: Tag TPSRate
  TFee                  :: Tag Lovelace
  TTTL                  :: Tag SlotNo
  TTxAdditionalSize     :: Tag TxAdditionalSize
  TLocalSocket          :: Tag String

deriving instance Show (Tag v)

deriveGEq ''Tag
deriveGCompare ''Tag
deriveGShow ''Tag
deriveArgDict ''Tag

data Sum where
  SInitCooldown         :: InitCooldown         -> Sum
  SNumberOfInputsPerTx  :: NumberOfInputsPerTx  -> Sum
  SNumberOfOutputsPerTx :: NumberOfOutputsPerTx -> Sum
  SNumberOfTxs          :: NumberOfTxs          -> Sum
  STPSRate              :: TPSRate              -> Sum
  SFee                  :: Lovelace             -> Sum
  STTL                  :: SlotNo               -> Sum
  STxAdditionalSize     :: TxAdditionalSize     -> Sum
  SLocalSocket          :: String               -> Sum
  deriving (Eq, Ord, Show, Generic)

taggedToSum :: Applicative f => DSum Tag f -> f Sum
taggedToSum x = case x of
  (TInitCooldown         :=> v) -> SInitCooldown         <$> v
  (TNumberOfInputsPerTx  :=> v) -> SNumberOfInputsPerTx  <$> v
  (TNumberOfOutputsPerTx :=> v) -> SNumberOfOutputsPerTx <$> v
  (TNumberOfTxs          :=> v) -> SNumberOfTxs          <$> v
  (TTPSRate              :=> v) -> STPSRate              <$> v
  (TFee                  :=> v) -> SFee                  <$> v
  (TTTL                  :=> v) -> STTL                  <$> v
  (TTxAdditionalSize     :=> v) -> STxAdditionalSize     <$> v
  (TLocalSocket          :=> v) -> SLocalSocket          <$> v

sumToTaggged :: Applicative f => Sum -> DSum Tag f
sumToTaggged x = case x of
  SInitCooldown         v -> (TInitCooldown         ==> v)
  SNumberOfInputsPerTx  v -> (TNumberOfInputsPerTx  ==> v)
  SNumberOfOutputsPerTx v -> (TNumberOfOutputsPerTx ==> v)
  SNumberOfTxs          v -> (TNumberOfTxs          ==> v)
  STPSRate              v -> (TTPSRate              ==> v)
  SFee                  v -> (TFee                  ==> v)
  STTL                  v -> (TTTL                  ==> v)
  STxAdditionalSize     v -> (TTxAdditionalSize     ==> v)
  SLocalSocket          v -> (TLocalSocket          ==> v)
