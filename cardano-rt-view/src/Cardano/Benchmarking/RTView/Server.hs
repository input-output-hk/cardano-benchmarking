{-# LANGUAGE OverloadedStrings   #-}

module Cardano.Benchmarking.RTView.Server
    ( launchServer
    ) where

import           Cardano.Prelude

import           Control.Concurrent.MVar
                   ( MVar )
import           Network.Socket
                   ( PortNumber )

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
                   ( UI
                   , (#), onEvent, set
                   )
import           Graphics.UI.Threepenny.Timer
                   ( start, tick, timer, interval )

import           Cardano.Benchmarking.RTView.NodeState.Types
                   ( NodesState )
import           Cardano.Benchmarking.RTView.GUI.Markup
                   ( mkPageBody )
import           Cardano.Benchmarking.RTView.GUI.Updater
                   ( updateGUI )

-- | Launch web server.
launchServer
  :: MVar NodesState
  -> FilePath
  -> PortNumber
  -> IO ()
launchServer nsMVar pathToStatic port =
  UI.startGUI config $ mainPage nsMVar
 where
  config = UI.defaultConfig
    { UI.jsStatic = Just pathToStatic
    , UI.jsPort   = Just $ fromIntegral port
    }

mainPage
  :: MVar NodesState
  -> UI.Window
  -> UI ()
mainPage nsMVar window = void $ do
  void $ return window # set UI.title "Cardano Node RTView"

  -- It is assumed that CSS files are available at 'pathToStatic/css/'.
  UI.addStyleSheet window "w3.css"
  UI.addStyleSheet window "cardano-rt-view.css"

  (pageBody, nodeStateElems) <- mkPageBody window

  -- Start the timer for GUI update. Every second it will
  -- call a function which updates node state elements on the page.
  guiUpdateTimer <- timer # set interval 1000 -- Every 1000 ms.
  void $ onEvent (tick guiUpdateTimer) $ \_ -> do
    newState <- liftIO $ readMVar nsMVar
    updateGUI newState nodeStateElems
  start guiUpdateTimer

  -- Create page's pody.
  UI.element pageBody
