{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.GeneratorTx.Benchmark
  ( Benchmark(..)
  , PartialBenchmark(..)
  , defaultBenchmark
  , mkBenchmark
  , parsePartialBenchmark

  , InitCooldown(..)
  , NumberOfInputsPerTx(..)
  , NumberOfOutputsPerTx(..)
  , NumberOfTxs(..)
  , TxAdditionalSize(..)
  , TPSRate(..)

  , Ack(..)
  , Acked(..)
  , ToAnnce(..)
  , Req(..)
  , Sent(..)
  , UnAcked(..)
  , Unav(..)
  , UnReqd(..)

  , SubmissionSummary(..)
  ) where

import           Prelude (String)
import           Cardano.Prelude hiding (TypeError)

import           Data.Aeson (ToJSON (..))
import qualified Data.List.NonEmpty as NE
import           Data.Monoid.Generic
import           Data.Time.Clock (NominalDiffTime)
import           Data.Word (Word64)
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

-- Era-agnostic imports
import           Ouroboros.Consensus.Block.Abstract (SlotNo(..))

-- Node API imports
import           Cardano.Api.Typed

-- Node imports
import           Cardano.Config.Types (NodeAddress)
import           Cardano.TracingOrphanInstances.Byron()
import           Cardano.TracingOrphanInstances.Common()
import           Cardano.TracingOrphanInstances.Consensus()
import           Cardano.TracingOrphanInstances.Mock()
import           Cardano.TracingOrphanInstances.Network()
import           Cardano.TracingOrphanInstances.Shelley()

import Cardano.Benchmarking.GeneratorTx.CLI.Parsers

{-------------------------------------------------------------------------------
  Ground types
-------------------------------------------------------------------------------}
-- | How long wait before starting the main submission phase,
--   after the init Tx batch was submitted.
newtype InitCooldown =
  InitCooldown Int
  deriving (Eq, Ord, Num, Show)

newtype NumberOfInputsPerTx =
  NumberOfInputsPerTx Int
  deriving (Eq, Ord, Num, Show)

newtype NumberOfOutputsPerTx =
  NumberOfOutputsPerTx Int
  deriving (Eq, Ord, Num, Show)

newtype NumberOfTxs =
  NumberOfTxs Word64
  deriving (Eq, Ord, Num, Show)

newtype TPSRate =
  TPSRate Double
  deriving (Eq, Generic, Ord, Num, Show)

-- | This parameter specifies additional size (in bytes) of transaction.
--   Since 1 transaction is ([input] + [output] + attributes), its size
--   is defined by its inputs and outputs. We want to have an ability to
--   increase transaction's size without increasing the number of inputs/
--   outputs. Such a big transaction will give us more real-world results
--   of benchmarking.
--   Technically this parameter specifies the size of attribute we'll
--   add to transaction (by default attributes are empty, so if this
--   parameter is skipped, attributes will remain empty).
newtype TxAdditionalSize =
  TxAdditionalSize { unTxAdditionalSize :: Int }
  deriving (Eq, Ord, Num, Show)

-- | Transactions not yet even announced.
newtype UnReqd  tx = UnReqd  [tx]

-- | Transactions we decided to announce now.
newtype ToAnnce tx = ToAnnce [tx]

-- | Transactions announced, yet unacked by peer.
newtype UnAcked tx = UnAcked [tx]

-- | Transactions acked by peer.
newtype Acked tx = Acked [tx]

-- | Peer acknowledged this many txids of the outstanding window.
newtype Ack = Ack Int deriving (Enum, Eq, Integral, Num, Ord, Real)

-- | Peer requested this many txids to add to the outstanding window.
newtype Req = Req Int deriving (Enum, Eq, Integral, Num, Ord, Real)

-- | This many Txs sent to peer.
newtype Sent = Sent Int deriving (Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

-- | This many Txs requested by the peer, but not available for sending.
newtype Unav = Unav Int deriving (Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

instance ToJSON Sent
instance ToJSON Unav
instance ToJSON TPSRate

deriving instance Num Lovelace

parseNumberOfTxs :: String -> String -> Parser NumberOfTxs
parseNumberOfTxs opt desc = NumberOfTxs <$> parseIntegral opt desc

parseNumberOfInputsPerTx :: String -> String -> Parser NumberOfInputsPerTx
parseNumberOfInputsPerTx opt desc = NumberOfInputsPerTx <$> parseIntegral opt desc

parseNumberOfOutputsPerTx :: String -> String -> Parser NumberOfOutputsPerTx
parseNumberOfOutputsPerTx opt desc = NumberOfOutputsPerTx <$> parseIntegral opt desc

parseTPSRate :: String -> String -> Parser TPSRate
parseTPSRate opt desc = TPSRate <$> parseDouble opt desc

parseInitCooldown :: String -> String -> Parser InitCooldown
parseInitCooldown opt desc = InitCooldown <$> parseIntegral opt desc

parseTxAdditionalSize :: String -> String -> Parser TxAdditionalSize
parseTxAdditionalSize opt desc = TxAdditionalSize <$> parseIntegral opt desc

-- | Summary of a tx submission run.
data SubmissionSummary
  = SubmissionSummary
    { ssTxSent        :: !Sent
    , ssTxUnavailable :: !Unav
    , ssElapsed       :: !NominalDiffTime
    , ssEffectiveTps  :: !TPSRate
    , ssThreadwiseTps :: ![TPSRate]
    } deriving (Show, Generic)
instance ToJSON SubmissionSummary

-- | Specification for a benchmark run.
data Benchmark
  = Benchmark
    { bTargets        :: !(NonEmpty NodeAddress)
    , bInitCooldown   :: !InitCooldown
    , bInitialTTL     :: !SlotNo
    , bTxCount        :: !NumberOfTxs
    , bTps            :: !TPSRate
    , bTxFanIn        :: !NumberOfInputsPerTx
    , bTxFanOut       :: !NumberOfOutputsPerTx
    , bTxFee          :: !Lovelace
    , bTxExtraPayload :: !TxAdditionalSize
    }
  deriving (Generic, Show)
-- Warning:  make sure to maintain correspondence between the two data structures.
data PartialBenchmark
  = PartialBenchmark
    { pbTargets        :: !(Last (NonEmpty NodeAddress))
    , pbInitCooldown   :: !(Last InitCooldown)
    , pbInitialTTL     :: !(Last SlotNo)
    , pbTxCount        :: !(Last NumberOfTxs)
    , pbTps            :: !(Last TPSRate)
    , pbTxFanIn        :: !(Last NumberOfInputsPerTx)
    , pbTxFanOut       :: !(Last NumberOfOutputsPerTx)
    , pbTxFee          :: !(Last Lovelace)
    , pbTxExtraPayload :: !(Last TxAdditionalSize)
    }
  deriving (Generic, Show)
  deriving Semigroup via GenericSemigroup PartialBenchmark
  deriving Monoid    via GenericMonoid PartialBenchmark

parsePartialBenchmark :: Opt.Parser PartialBenchmark
parsePartialBenchmark =
  PartialBenchmark
    <$> lastly (NE.fromList <$> some (
            parseTargetNodeAddress
              "target-node"
              "IP address and port of the node transactions will be sent to."
          )
        )
    <*> (lastly $ parseInitCooldown
          "init-cooldown"
          "Delay between init and main submission phases.")
    <*> (lastly $ parseInitialTTL
          "initial-ttl"
          "Slot denoting TTL of the initial transactions.")
    <*> (lastly $ parseNumberOfTxs
         "num-of-txs"
         "Number of transactions generator will create.")
    <*> (lastly $ parseTPSRate
          "tps"
          "TPS (transaction per second) rate.")
    <*> (lastly $ parseNumberOfInputsPerTx
          "inputs-per-tx"
          "Number of inputs in each of transactions.")
    <*> (lastly $ parseNumberOfOutputsPerTx
          "outputs-per-tx"
          "Number of outputs in each of transactions.")
    <*> (lastly $ parseFeePerTx
          "tx-fee"
          "Fee per transaction, in Lovelaces.")
    <*> (lastly $ parseTxAdditionalSize
          "add-tx-size"
          "Additional size of transaction, in bytes.")

defaultBenchmark :: PartialBenchmark
defaultBenchmark =
  PartialBenchmark
  { pbTargets        = mempty
  , pbInitCooldown   = pure 20
  , pbInitialTTL     = pure (SlotNo 100000000)
  , pbTxCount        = pure 1000
  , pbTps            = pure 10
  , pbTxFanIn        = pure 1
  , pbTxFanOut       = pure 1
  , pbTxFee          = pure 1000
  , pbTxExtraPayload = pure 100
  }

-- This is called at the last stage of the Partial Options Monoid approach.
-- https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67
mkBenchmark :: PartialBenchmark -> Either Text Benchmark
mkBenchmark PartialBenchmark{..} = do
  bTargets        <- mkComplete "bTargets       " pbTargets
  bInitCooldown   <- mkComplete "bInitCooldown  " pbInitCooldown
  bInitialTTL     <- mkComplete "bInitialTTL    " pbInitialTTL
  bTxCount        <- mkComplete "bTxCount       " pbTxCount
  bTps            <- mkComplete "bTps           " pbTps
  bTxFanIn        <- mkComplete "bTxFanIn       " pbTxFanIn
  bTxFanOut       <- mkComplete "bTxFanOut      " pbTxFanOut
  bTxFee          <- mkComplete "bTxFee         " pbTxFee
  bTxExtraPayload <- mkComplete "bTxExtraPayload" pbTxExtraPayload
  pure Benchmark{..}
 where
   -- | Return an error if the @Last@ option is incomplete.
   mkComplete :: Text -> Last a -> Either Text a
   mkComplete err (Last x) = maybe (Left err) Right x
