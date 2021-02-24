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

import           Cardano.Benchmarking.Script.Setters as Setters
import           Cardano.Benchmarking.OuroborosImports as Cardano
                    ( Protocol, CardanoBlock, ProtocolCardano, LoggingLayer, ShelleyGenesis, StandardShelley
                    , NetworkId, SigningKey, PaymentKey)
import           Cardano.Benchmarking.Tracer as Core (BenchTracers)

type Name = String

data Store v where
  User   :: Setters.Tag x -> Store x 
  LoggingLayer :: Store LoggingLayer
  Protocol     :: Store (Cardano.Protocol IO CardanoBlock ProtocolCardano)
  BenchTracers :: Store Core.BenchTracers
  NetworkId    :: Store Cardano.NetworkId -- could be in Setters (just need JSON instance)
  Genesis      :: Store (ShelleyGenesis StandardShelley)
  NamedKey     :: Name -> Store (SigningKey PaymentKey)

deriveGEq ''Store
deriveGCompare ''Store
deriveGShow ''Store
deriveArgDict ''Store

deriving instance Show (Store v)
