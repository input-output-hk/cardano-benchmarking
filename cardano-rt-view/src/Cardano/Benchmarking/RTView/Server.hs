{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.Server
    ( launchServer
    ) where

import           Cardano.Prelude

import           Network.Socket
                   ( PortNumber )

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
                   ( UI
                   , (#), onEvent, set
                   )
import           Graphics.UI.Threepenny.Timer
                   ( start, tick, timer, interval )

import           Cardano.BM.Data.Configuration
                   ( RemoteAddrNamed (..) )
import           Cardano.Benchmarking.RTView.NodeState.Types
                   ( NodesState )
import           Cardano.Benchmarking.RTView.GUI.Markup
                   ( mkPageBody )
import           Cardano.Benchmarking.RTView.GUI.Updater
                   ( updateGUI )

-- | Launch web server.
launchServer
  :: MVar Text
  -> MVar NodesState
  -> FilePath
  -> PortNumber
  -> [RemoteAddrNamed]
  -> IO ()
launchServer activeNodeMVar nsMVar pathToStatic port acceptors =
  UI.startGUI config $ mainPage activeNodeMVar nsMVar acceptors
 where
  config = UI.defaultConfig
    { UI.jsStatic = Just pathToStatic
    , UI.jsPort   = Just $ fromIntegral port
    }

mainPage
  :: MVar Text
  -> MVar NodesState
  -> [RemoteAddrNamed]
  -> UI.Window
  -> UI ()
mainPage activeNodeMVar nsMVar acceptors window = do
  void $ return window # set UI.title "Cardano Node RTView"

  -- It is assumed that CSS files are available at 'pathToStatic/css/'.
  UI.addStyleSheet window "w3.css"
  UI.addStyleSheet window "cardano-rt-view.css"

  -- Make page's body (HTML markup).
  (pageBody, nodeStateElems) <- mkPageBody window activeNodeMVar acceptors

  -- Start the timer for GUI update. Every second it will
  -- call a function which updates node state elements on the page.
  guiUpdateTimer <- timer # set interval 800 -- Every 0.8 s.
  void $ onEvent (tick guiUpdateTimer) $ \_ -> do
    newState <- liftIO $ readMVar nsMVar
    activeNode <- liftIO $ readMVar activeNodeMVar
    updateGUI newState activeNode acceptors nodeStateElems
  start guiUpdateTimer

  void $ UI.element pageBody
