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

module Cardano.Benchmarking.Script.Store
where

import           Prelude

import           Data.Constraint.Extras.TH (deriveArgDict)
import           Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import           Data.GADT.Show.TH (deriveGShow)

import           Cardano.Api as Cardano (InAnyCardanoEra(..), Tx)
import           Cardano.Benchmarking.Script.Setters as Setters
import           Cardano.Benchmarking.OuroborosImports as Cardano
                    ( Protocol, CardanoBlock, ProtocolCardano, LoggingLayer, ShelleyGenesis, StandardShelley
                    , NetworkId, SigningKey, PaymentKey)

import           Cardano.Benchmarking.Tracer as Core (BenchTracers)
import           Cardano.Benchmarking.GeneratorTx as Core (AsyncBenchmarkControl)
import qualified Cardano.Benchmarking.GeneratorTx.Tx as Core (Fund)

type Fund = (Core.Fund, SigningKey PaymentKey)

data Store v where
  User         :: Setters.Tag x -> Store x
  LoggingLayer :: Store LoggingLayer
  Protocol     :: Store (Cardano.Protocol IO CardanoBlock ProtocolCardano)
  BenchTracers :: Store Core.BenchTracers
  NetworkId    :: Store Cardano.NetworkId -- could be in Setters (just need JSON instance)
  Genesis      :: Store (ShelleyGenesis StandardShelley)
  Named        :: Name x -> Store x

data Name x where
  KeyName      :: !String -> Name (SigningKey PaymentKey)
  FundName     :: !String -> Name Fund
  FundListName :: !String -> Name [Fund]
  TxListName   :: !String -> Name (InAnyCardanoEra TxList)
  ThreadName   :: !String -> Name AsyncBenchmarkControl

type KeyName      = Name (SigningKey PaymentKey)
type FundName     = Name Fund
type FundListName = Name [Fund]
type TxListName   = Name (InAnyCardanoEra TxList)
type ThreadName   = Name AsyncBenchmarkControl

newtype TxList era = TxList [Tx era]

-- Remember when debugging at 4:00AM :
-- TH-Haskell is imperative: It breaks up Main into smaller binding groups!
-- This means declarations below a splice are not visible above.
-- The order of splices & declarations matters.

deriveGEq ''Name
deriveGCompare ''Name
deriveGShow ''Name
deriveArgDict ''Name
deriving instance Show (Name x)
deriving instance Eq (Name x)

deriveGEq ''Store
deriveGCompare ''Store
deriveGShow ''Store
deriveArgDict ''Store

deriving instance Show (Store v)
deriving instance Eq (Store x)
