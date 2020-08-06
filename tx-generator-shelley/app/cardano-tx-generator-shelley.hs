{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans.Except.Extra (runExceptT)
import           Options.Applicative (ParserInfo, customExecParser, fullDesc, header, helper, info,
                                      prefs, showHelpOnEmpty, (<**>))
import           System.Exit (exitFailure)

import           Cardano.Benchmarking.TxGenerator.CLI.Parsers (GenerateTxs, parseCommand)
import           Cardano.Benchmarking.TxGenerator.CLI.Run (runCommand)

main :: IO ()
main = do
  generateTxs <- customExecParser (prefs showHelpOnEmpty) txGenInfo
  runExceptT (runCommand generateTxs) >>= \case
    Right _  -> pure ()
    Left err -> print err >> exitFailure
 where
  txGenInfo :: ParserInfo GenerateTxs
  txGenInfo =
    info (parseCommand <**> helper)
         (fullDesc <> header "cardano-tx-generator-shelley - a transaction generator Shelley.")
