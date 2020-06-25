module Cardano.Benchmarking.GeneratorTx.Error
  ( TxGenError (..)
  ) where

import           Cardano.Prelude

data TxGenError =
    CurrentlyCannotSendTxToRelayNode !FilePath
  -- ^ Relay nodes cannot currently be transaction recipients.
  | InsufficientFundsForRecipientTx
  -- ^ Error occurred while creating the target node address.
  | NeedMinimumThreeSigningKeyFiles ![FilePath]
  -- ^ Need at least 3 signing key files.
  | TooSmallTPSRate !Double
  -- ^ TPS is less than lower limit.
  | SecretKeyDeserialiseError !Text
  | SecretKeyReadError !Text
  deriving Show
