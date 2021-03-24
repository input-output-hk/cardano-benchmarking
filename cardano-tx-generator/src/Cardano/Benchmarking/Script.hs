{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
module Cardano.Benchmarking.Script
(
    Script
  , runScript
  , parseScriptFile
  )
where

import           Prelude

import           Control.Concurrent (threadDelay)
import           Control.Monad

import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Aeson (parseScriptFile)
import           Cardano.Benchmarking.Script.Env

type Script = [Action]

runScript :: Script -> IOManager -> IO (Either Error ())
runScript script iom = runActionM (forM_ script action) iom >>= \case
    (Right a , _s ,  ()) -> return $ Right a
    (Left err , s  , ())  -> do
       _ <- runActionMEnv s (traceError $ show err) iom
       threadDelay $ 10_000_000
       return $ Left err
