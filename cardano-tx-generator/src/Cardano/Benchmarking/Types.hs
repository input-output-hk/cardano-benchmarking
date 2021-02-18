{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.Types
  (
    NodeIPv4Address
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
  , SubmissionErrorPolicy(..)
  ) where


import           Prelude
import           Data.Word
import           GHC.Generics

-- Node API imports
import           Cardano.Api

import           Cardano.Node.Types (NodeIPv4Address)

-- | How long wait before starting the main submission phase,
--   after the init Tx batch was submitted.
newtype InitCooldown =
  InitCooldown Int
  deriving newtype (Eq, Ord, Num, Show)

newtype NumberOfInputsPerTx =
  NumberOfInputsPerTx Int
  deriving newtype (Eq, Ord, Num, Show)

newtype NumberOfOutputsPerTx =
  NumberOfOutputsPerTx Int
  deriving newtype (Eq, Ord, Num, Show)

newtype NumberOfTxs =
  NumberOfTxs { unNumberOfTxs :: Word64 }
  deriving newtype (Eq, Ord, Num, Show)

newtype TPSRate =
  TPSRate Double
  deriving newtype (Eq, Ord, Num, Show)
deriving stock instance Generic TPSRate

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
  deriving newtype (Eq, Ord, Num, Show)

-- | Transactions not yet even announced.
newtype UnReqd  tx = UnReqd  [tx]

-- | Transactions we decided to announce now.
newtype ToAnnce tx = ToAnnce [tx]

-- | Transactions announced, yet unacked by peer.
newtype UnAcked tx = UnAcked [tx]

-- | Transactions acked by peer.
newtype Acked tx = Acked [tx]

-- | Peer acknowledged this many txids of the outstanding window.
newtype Ack = Ack Int deriving newtype (Enum, Eq, Integral, Num, Ord, Real)

-- | Peer requested this many txids to add to the outstanding window.
newtype Req = Req Int deriving newtype (Enum, Eq, Integral, Num, Ord, Real)

-- | This many Txs sent to peer.
newtype Sent = Sent Int deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show)
deriving stock instance Generic Sent

-- | This many Txs requested by the peer, but not available for sending.
newtype Unav = Unav Int deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show)
deriving stock instance Generic Unav

data SubmissionErrorPolicy
  = FailOnError
  | LogErrors
  deriving stock (Eq, Show)

instance ToJSON Sent
instance ToJSON Unav
instance ToJSON TPSRate
