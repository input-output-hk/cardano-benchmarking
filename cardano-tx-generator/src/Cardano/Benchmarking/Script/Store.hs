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
import           Cardano.Benchmarking.OuroborosImports as Cardano (Protocol, CardanoBlock, ProtocolCardano)

data Store v where
  User   :: Setters.Tag x -> Store x 
  LoggingLayer :: Store ()
  Protocol     :: Store (Cardano.Protocol IO CardanoBlock ProtocolCardano)    

deriveGEq ''Store
deriveGCompare ''Store
deriveGShow ''Store
deriveArgDict ''Store
