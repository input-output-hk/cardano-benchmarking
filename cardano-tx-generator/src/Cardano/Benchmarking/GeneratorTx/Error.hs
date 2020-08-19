{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Benchmarking.GeneratorTx.Error
  ( TxGenError (..)
  ) where

import           Cardano.Api.Typed
import           Cardano.Prelude

data TxGenError =
    InsufficientFundsForRecipientTx !Lovelace !Lovelace
  -- ^ The calculated expenditure (second value) was not available as a single
  --   UTxO entry.  The first value is the largest single UTxO available.
  | TxFileError !(FileError TextEnvelopeError)
  | SplittingSubmissionError !Text
  | UtxoReadFailure !Text
  | SuppliedUtxoTooSmall !Int !Int
  -- ^ The supplied UTxO size (second value) was less than the requested
  --   number of transactions to send (first value).
  deriving Show
