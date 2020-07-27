module Cardano.Benchmarking.TxGenerator.Error
  ( TxGenError (..)
  ) where

import           Prelude
import           Cardano.Api.Typed
import           Cardano.Binary (DecoderError)
import           Cardano.Benchmarking.TxGenerator.Producer

data TxGenError =
    CurrentlyCannotSendTxToRelayNode !FilePath
  -- ^ Relay nodes cannot currently be transaction recipients.
  | InsufficientFundsForRecipientTx
  -- ^ Error occurred while creating the target node address.
  | NeedMinimumThreeSigningKeyFiles ![FilePath]
  -- ^ Need at least 3 signing key files.
  | TooSmallTPSRate !Float
  -- ^ TPS is less than lower limit.
  | UTxOParseError !String
  | AddrParseError !DecoderError
  | TxFileError !(FileError TextEnvelopeError)
  | TxSubmitError !String
  | Phase1SplitError !PError
  deriving Show
