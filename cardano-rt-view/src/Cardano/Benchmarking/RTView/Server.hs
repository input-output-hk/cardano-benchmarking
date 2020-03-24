{-# LANGUAGE OverloadedStrings   #-}

module Cardano.Benchmarking.RTView.Server
    ( launchServer
    ) where

import           Cardano.Prelude

import           Control.Concurrent.MVar
                   ( MVar )
import           Network.Socket
                   ( PortNumber )
import           Data.Map.Strict
                   ( (!) )
import           Data.Time.Calendar
                   ( Day (..) )
import           Data.Time.Clock
                   ( UTCTime (..)
                   , addUTCTime, diffUTCTime, getCurrentTime
                   )
import           Data.Time.Format
                   ( defaultTimeLocale, formatTime )

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
                   ( UI
                   , (#), onEvent, set
                   )
import           Graphics.UI.Threepenny.Timer
                   ( start, tick, timer, interval )

import           Cardano.Benchmarking.RTView.NodeState
                   ( NodeInfo (..), NodeMetrics (..), NodeState (..)
                   , defaultNodeState
                   )
import           Cardano.Benchmarking.RTView.GUI.Body
                   ( mkPageBody )
import           Cardano.Benchmarking.RTView.GUI.NodeStateElements
                   ( ElementName (..), ElementValue (..), NodeStateElements
                   , updateElementValue, updateProgressBar
                   )

-- | Launch web server.
launchServer
  :: MVar NodeState
  -> FilePath
  -> PortNumber
  -> IO ()
launchServer nodeStateMVar pathToStatic port =
  UI.startGUI config $ mainPage nodeStateMVar
 where
  config = UI.defaultConfig
    { UI.jsStatic = Just pathToStatic
    , UI.jsPort   = Just $ fromIntegral port
    , UI.jsAddr   = Nothing -- Just hostAddr
    }

mainPage
  :: MVar NodeState
  -> UI.Window
  -> UI ()
mainPage nodeStateMVar window = void $ do
  void $ return window # set UI.title "Cardano Node RTView"

  -- It is assumed that CSS files are available at 'pathToStatic/css/'.
  UI.addStyleSheet window "w3.css"
  UI.addStyleSheet window "cardano-rt-view.css"

  (pageBody, nodeStateElems) <- mkPageBody window

  -- Start the timer for GUI update. Every second it will
  -- call a function which updates node state elements on the page.
  guiUpdateTimer <- timer # set interval 1000 -- Every 1000 ms.
  void $ onEvent (tick guiUpdateTimer) $ \_ -> do
    maybeState <- liftIO $ tryReadMVar nodeStateMVar
    newState <- case maybeState of
                  Just ns -> return ns
                  Nothing -> do
                    now <- liftIO getCurrentTime
                    return $ defaultNodeState now
    updateGUI newState nodeStateElems
  start guiUpdateTimer

  UI.element pageBody
 where
  updateGUI :: NodeState -> NodeStateElements -> UI ()
  updateGUI nodeState elements = do
    let ni = nsInfo nodeState
        nm = nsMetrics nodeState
    
    now <- liftIO getCurrentTime
    let diffBetweenNowAndStart = diffUTCTime now (niStartTime ni)
        upTimeHMS = formatTime defaultTimeLocale "%X" $
                      addUTCTime diffBetweenNowAndStart (UTCTime (ModifiedJulianDay 0) 0)
        mempoolTxsMaxDouble :: Double
        mempoolTxsMaxDouble = fromIntegral $ nmMempoolTxsMax nm

    void $ updateElementValue (ElementString upTimeHMS)                $ elements ! ElUptime
    void $ updateElementValue (ElementInt    $ niEpoch ni)             $ elements ! ElEpoch
    void $ updateElementValue (ElementInt    $ niSlot ni)              $ elements ! ElSlot
    void $ updateElementValue (ElementInt    $ niBlocksNumber ni)      $ elements ! ElBlocksNumber
    void $ updateElementValue (ElementDouble $ niChainDensity ni)      $ elements ! ElChainDensity
    void $ updateElementValue (ElementInt    $ niTxsProcessed ni)      $ elements ! ElTxsProcessed
    void $ updateElementValue (ElementInt    $ niPort ni)              $ elements ! ElPort
    void $ updateElementValue (ElementInt    $ niPeersNumber ni)       $ elements ! ElPeersNumber
    void $ updateElementValue (ElementDouble $ nmMempoolKBMax nm)      $ elements ! ElMempoolKBMax
    void $ updateElementValue (ElementDouble $ nmMempoolKBPercent nm)  $ elements ! ElMempoolKBPercent
    void $ updateElementValue (ElementInt    $ nmMempoolTxsMax nm)     $ elements ! ElMempoolTxsMax
    void $ updateElementValue (ElementDouble $ nmMempoolTxsPercent nm) $ elements ! ElMempoolTxsPercent
    void $ updateElementValue (ElementDouble $ nmMemoryMax nm)         $ elements ! ElMemoryMax
    void $ updateElementValue (ElementDouble $ nmMemoryPercent nm)     $ elements ! ElMemoryPercent
    void $ updateElementValue (ElementDouble $ nmCPUMax nm)            $ elements ! ElCPUMax
    void $ updateElementValue (ElementDouble $ nmCPUPercent nm)        $ elements ! ElCPUPercent
    void $ updateElementValue (ElementDouble $ nmDiskReadMax nm)       $ elements ! ElDiskReadMax
    void $ updateElementValue (ElementDouble $ nmDiskReadPercent nm)   $ elements ! ElDiskReadPercent
    void $ updateElementValue (ElementDouble $ nmDiskWriteMax nm)      $ elements ! ElDiskWriteMax
    void $ updateElementValue (ElementDouble $ nmDiskWritePercent nm)  $ elements ! ElDiskWritePercent
    void $ updateElementValue (ElementDouble $ nmNetworkInMax nm)      $ elements ! ElNetworkInMax
    void $ updateElementValue (ElementDouble $ nmNetworkInPercent nm)  $ elements ! ElNetworkInPercent
    void $ updateElementValue (ElementDouble $ nmNetworkOutMax nm)     $ elements ! ElNetworkOutMax
    void $ updateElementValue (ElementDouble $ nmNetworkOutPercent nm) $ elements ! ElNetworkOutPercent
    void $ updateElementValue (ElementDouble $ nmNetworkOutPercent nm) $ elements ! ElNetworkOutPercent
   
    void $ updateProgressBar (nmMempoolKBPercent nm)  (nmMempoolKBMax nm)   $ elements ! ElMempoolKBProgress
    void $ updateProgressBar (nmMempoolTxsPercent nm) (mempoolTxsMaxDouble) $ elements ! ElMempoolTxsProgress
    void $ updateProgressBar (nmMemoryPercent nm)     (nmMemoryMax nm)      $ elements ! ElMemoryProgress
    void $ updateProgressBar (nmCPUPercent nm)        (nmCPUMax nm)         $ elements ! ElCPUProgress
    void $ updateProgressBar (nmDiskReadPercent nm)   (nmDiskReadMax nm)    $ elements ! ElDiskReadProgress
    void $ updateProgressBar (nmDiskWritePercent nm)  (nmDiskWriteMax nm)   $ elements ! ElDiskWriteProgress
    void $ updateProgressBar (nmNetworkInPercent nm)  (nmNetworkInMax nm)   $ elements ! ElNetworkInProgress
    void $ updateProgressBar (nmNetworkOutPercent nm) (nmNetworkOutMax nm)  $ elements ! ElNetworkOutProgress
