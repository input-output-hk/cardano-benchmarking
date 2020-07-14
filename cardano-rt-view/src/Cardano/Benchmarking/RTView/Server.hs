{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.Server
    ( launchServer
    ) where

import           Cardano.Prelude hiding ( readMVar )

import           Control.Concurrent.MVar.Strict
                   ( MVar
                   , readMVar
                   )
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
                   ( UI
                   , (#), (#+), onEvent, set
                   )
import           Graphics.UI.Threepenny.Timer
                   ( start, tick, timer, interval )

import           Cardano.BM.Data.Configuration
                   ( RemoteAddrNamed (..) )

import           Cardano.Benchmarking.RTView.CLI
                   ( RTViewParams (..) )
import           Cardano.Benchmarking.RTView.NodeState.Types
                   ( NodesState )
import           Cardano.Benchmarking.RTView.GUI.Markup
                   ( mkPageBody )
import           Cardano.Benchmarking.RTView.GUI.Updater
                   ( updateGUI )

-- | Launch web server.
launchServer
  :: MVar NodesState
  -> RTViewParams
  -> [RemoteAddrNamed]
  -> IO ()
launchServer nsMVar params acceptors =
  UI.startGUI config $ mainPage nsMVar params acceptors
 where
  config = UI.defaultConfig
    { UI.jsStatic = Just $ rtvStatic params
    , UI.jsPort   = Just $ fromIntegral (rtvPort params)
    }

mainPage
  :: MVar NodesState
  -> RTViewParams
  -> [RemoteAddrNamed]
  -> UI.Window
  -> UI ()
mainPage nsMVar params acceptors window = do
  void $ return window # set UI.title "Cardano Node RTView"

  -- It is assumed that CSS files are available at 'pathToStatic/css/'.
  UI.addStyleSheet window "w3.css"
  UI.addStyleSheet window "cardano-rt-view.css"

  -- It is assumed that JS files are available at 'pathToStatic/js/'.
  addJavaScript window "chart.js"

  -- Make page's body (HTML markup).
  (pageBody, (nodesStateElems, gridNodesStateElems)) <- mkPageBody window acceptors

  -- Start the timer for GUI update. Every second it will
  -- call a function which updates node state elements on the page.
  guiUpdateTimer <- timer # set interval 2000 -- Every 2 s.
  void $ onEvent (tick guiUpdateTimer) $ \_ -> do
    newState <- liftIO $ readMVar nsMVar
    updateGUI window newState params acceptors (nodesStateElems, gridNodesStateElems)
  start guiUpdateTimer

  void $ UI.element pageBody

-- | ...
addJavaScript
  :: UI.Window
  -> FilePath
  -> UI ()
addJavaScript w filename = void $ do
  el <- UI.mkElement "script" # set UI.src ("/static/js/" ++ filename)
  UI.getHead w #+ [UI.element el]
