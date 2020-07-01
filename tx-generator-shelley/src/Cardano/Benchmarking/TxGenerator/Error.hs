module Cardano.Benchmarking.TxGenerator.Error
  ( TxGenError (..)
  ) where

import           Prelude
import           Cardano.Api (ApiError)
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
--  | SecretKeyDeserialiseError !Text
--  | SecretKeyReadError !Text
  | UTxOParseError !String
  | AddrParseError !DecoderError
  | CardanoApiError !ApiError
  | TxSubmitError !String
  | Phase1SplitError !PError
  -- ^ error from Cardana.Api.Error
  deriving Show
