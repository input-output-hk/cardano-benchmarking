{-# Language DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Benchmarking.GeneratorTx.Error
  (
    TxGenError (..)
  ) where

import           Cardano.Api
import           Cardano.Prelude

data TxGenError =
    InsufficientFundsForRecipientTx !Lovelace !Lovelace
  -- ^ The calculated expenditure (second value) was not available as a single
  --   UTxO entry.  The first value is the largest single UTxO available.
  | TxFileError !(FileError TextEnvelopeError)
  | SplittingSubmissionError !Text
  | SuppliedUtxoTooSmall !Int !Int
  | BadPayloadSize !Text
  -- ^ The supplied UTxO size (second value) was less than the requested
  --   number of transactions to send (first value).
  deriving stock Show
