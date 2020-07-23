{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude hiding (option)

import           Control.Monad.Trans.Except.Extra (runExceptT)
import qualified Options.Applicative as Opt
import           System.Exit (exitFailure)

import           Cardano.Benchmarking.Run

main :: IO ()
main = do
  cmd <- Opt.customExecParser
         (Opt.prefs Opt.showHelpOnEmpty)
         (parserInfo
           "cardano-tx-generator - load Cardano clusters with parametrised transaction flow")
  runExceptT (runCommand cmd) >>= \case
    Right _  -> pure ()
    Left err -> print err >> exitFailure
