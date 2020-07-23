{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Benchmarking.GeneratorTx.Error
  ( TxGenError (..)
  ) where

import           Cardano.Prelude
import           Cardano.Api.Typed

data TxGenError =
    CurrentlyCannotSendTxToRelayNode !FilePath
  -- ^ Relay nodes cannot currently be transaction recipients.
  | InsufficientFundsForRecipientTx
  -- ^ Error occurred while creating the target node address.
  | NeedMinimumThreeSigningKeyFiles ![FilePath]
  -- ^ Need at least 3 signing key files.
  | TooSmallTPSRate !Double
  -- ^ TPS is less than lower limit.
  | TxFileError !(FileError TextEnvelopeError)
  | SecretKeyDeserialiseError !Text
  | SecretKeyReadError !Text
  | SplittingSubmissionError !Text
  deriving Show
