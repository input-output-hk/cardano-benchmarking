module Cardano.Benchmarking.Util
  ( putStrLnStderr
  ) where

import           Cardano.Prelude

import System.IO (stderr)

-- | Unbuffered variant of 'putStrLn', that prints to stderr.
--   Useful for debugging IO-related issues.
putStrLnStderr :: Text -> IO ()
putStrLnStderr = hPutStrLn stderr
