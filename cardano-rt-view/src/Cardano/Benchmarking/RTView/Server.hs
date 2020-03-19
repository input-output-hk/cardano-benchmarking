module Cardano.Benchmarking.RTView.Server
    ( launchServer
    ) where

import           Cardano.Prelude

import           Snap.Core
                   ( Snap )
import           Snap.Http.Server
                   ( Config
                   , defaultConfig, simpleHttpServe
                   )

-- | Launch web server.
launchServer :: IO ()
launchServer = do
  simpleHttpServe serverConfig handler
 where
  serverConfig :: Config Snap a
  serverConfig = defaultConfig

  handler :: Snap ()
  handler = return ()
