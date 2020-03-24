{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.GUI.Body
    ( mkPageBody
    ) where

import           Cardano.Prelude
import           Prelude
                   ( String )
import           Data.Text
                   ( unpack )
import qualified Data.Map.Strict as Map

import           Cardano.Config.GitRev
                   ( gitRev )

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
                   ( Element, UI
                   , (#), (#+), (#.)
                   , element, set, string
                   )

import           Cardano.Benchmarking.RTView.GUI.NodeStateElements
                   ( ElementName (..)
                   , NodeStateElements
                   )

mkPageBody
  :: UI.Window
  -> UI (Element, NodeStateElements)
mkPageBody window = do
  let cardanoNodeCommit = unpack gitRev
      shortCommit       = take 7 cardanoNodeCommit

  -- Create |Element|s containing node state (info, metrics).
  -- These elements will be part of the complete page,
  -- later they will be updated by acceptor thread.
  elUptime            <- string "00:00:00"
  elEpoch             <- string "0"
  elSlot              <- string "0"
  elBlocksNumber      <- string "0"
  elChainDensity      <- string "0"
  elTxsProcessed      <- string "0"
  elPort              <- string "0"
  elPeersNumber       <- string "0"
  elMempoolKBMax      <- string "0"
  elMempoolKBPercent  <- string "10"
  elMempoolTxsMax     <- string "0"
  elMempoolTxsPercent <- string "10"
  elMemoryMax         <- string "0"
  elMemoryPercent     <- string "10"
  elCPUMax            <- string "0"
  elCPUPercent        <- string "10"
  elDiskReadMax       <- string "0"
  elDiskReadPercent   <- string "10"
  elDiskWriteMax      <- string "0"
  elDiskWritePercent  <- string "10"
  elNetworkInMax      <- string "0"
  elNetworkInPercent  <- string "10"
  elNetworkOutMax     <- string "0"
  elNetworkOutPercent <- string "10"

  elMempoolKBProgress  <- UI.div #. "w3-teal" #+ [UI.span #. "h-spacer" #+ [], element elMempoolKBPercent]
  elMempoolTxsProgress <- UI.div #. "w3-teal" #+ [UI.span #. "h-spacer" #+ [], element elMempoolTxsPercent]
  elMemoryProgress     <- UI.div #. "w3-teal" #+ [UI.span #. "h-spacer" #+ [], element elMemoryPercent,     string "MB"]
  elCPUProgress        <- UI.div #. "w3-teal" #+ [UI.span #. "h-spacer" #+ [], element elCPUPercent,        string "%"]
  elDiskReadProgress   <- UI.div #. "w3-teal" #+ [UI.span #. "h-spacer" #+ [], element elDiskReadPercent,   string "KB/s"]
  elDiskWriteProgress  <- UI.div #. "w3-teal" #+ [UI.span #. "h-spacer" #+ [], element elDiskWritePercent,  string "KB/s"]
  elNetworkInProgress  <- UI.div #. "w3-teal" #+ [UI.span #. "h-spacer" #+ [], element elNetworkInPercent,  string "KB/s"]
  elNetworkOutProgress <- UI.div #. "w3-teal" #+ [UI.span #. "h-spacer" #+ [], element elNetworkOutPercent, string "KB/s"]

  body <- UI.getBody window #+
    [ topNavigation
    , mainContainer
        [ UI.div #. "w3-row main-container-inner" #+
            [ UI.div #. "w3-twothird w3-theme" #+
                [ twoElementsInRow
                    (progressBar "Mempool (KB):"  "" elMempoolKBMax  elMempoolKBProgress)
                    (progressBar "Mempool (TXs):" "" elMempoolTxsMax elMempoolTxsProgress)
                , vSpacer "node-metrics-v-spacer"
                , progressBar "Memory usage:" " MB" elMemoryMax elMemoryProgress
                , vSpacer "node-metrics-v-spacer"
                , progressBar "CPU usage:" "%" elCPUMax elCPUProgress
                , vSpacer "node-metrics-v-spacer"
                , twoElementsInRow
                    (progressBar "Disk (RD):" " KB/s" elDiskReadMax  elDiskReadProgress)
                    (progressBar "Disk (WR):" " KB/s" elDiskWriteMax elDiskWriteProgress)
                , vSpacer "node-metrics-v-spacer"
                , twoElementsInRow
                    (progressBar "Network (IN):"  " KB/s" elNetworkInMax  elNetworkInProgress)
                    (progressBar "Network (OUT):" " KB/s" elNetworkOutMax elNetworkOutProgress)
                ]
            , UI.div #. "w3-third w3-container w3-theme" #+
                [ UI.div #. "w3-row" #+
                    [ UI.div #. "w3-twothird w3-container w3-theme" #+
                        [ UI.div #. "" #+
                            [ UI.div #. "" #+ [string "node commit:"]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [string "uptime:"]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [string "epoch / slot:"]
                            , UI.div #. "" #+ [string "blocks number:"]
                            , UI.div #. "" #+ [string "chain density:"]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [string "TXs processed:"]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [string "port:"]
                            , UI.div #. "" #+ [string "peers:"]
                            ]
                        ]
                    , UI.div #. "w3-third w3-container w3-theme" #+
                        [ UI.div #. "node-info-values" #+
                            [ UI.div #. "commit-link" #+
                                [ UI.anchor # set UI.href (cardanoNodeCommitUrl cardanoNodeCommit)
                                            # set UI.target "_blank"
                                            # set UI.title__ "Browse cardano-node repository on this commit"
                                            #+ [string shortCommit]
                                ]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [element elUptime]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+
                                [ element elEpoch
                                , string " / "
                                , element elSlot
                                ]
                            , UI.div #. "" #+ [element elBlocksNumber]
                            , UI.div #. "" #+ [element elChainDensity]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [element elTxsProcessed]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [element elPort]
                            , UI.div #. "" #+ [element elPeersNumber]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

  -- Store these elements, they will be updated by another thread later.
  let nodeStateElems =
        Map.fromList
          [ (ElUptime,             elUptime)
          , (ElEpoch,              elEpoch)
          , (ElSlot,               elSlot)
          , (ElBlocksNumber,       elBlocksNumber)
          , (ElChainDensity,       elChainDensity)
          , (ElTxsProcessed,       elTxsProcessed)
          , (ElPort,               elPort)
          , (ElPeersNumber,        elPeersNumber)
          , (ElMempoolKBMax,       elMempoolKBMax)
          , (ElMempoolKBPercent,   elMempoolKBPercent)
          , (ElMempoolTxsMax,      elMempoolTxsMax)
          , (ElMempoolTxsPercent,  elMempoolTxsPercent)
          , (ElMemoryMax,          elMemoryMax)
          , (ElMemoryPercent,      elMemoryPercent)
          , (ElCPUMax,             elCPUMax)
          , (ElCPUPercent,         elCPUPercent)
          , (ElDiskReadMax,        elDiskReadMax)
          , (ElDiskReadPercent,    elDiskReadPercent)
          , (ElDiskWriteMax,       elDiskWriteMax)
          , (ElDiskWritePercent,   elDiskWritePercent)
          , (ElNetworkInMax,       elNetworkInMax)
          , (ElNetworkInPercent,   elNetworkInPercent)
          , (ElNetworkOutMax,      elNetworkOutMax)
          , (ElNetworkOutPercent,  elNetworkOutPercent)
          , (ElMempoolKBProgress,  elMempoolKBProgress)
          , (ElMempoolTxsProgress, elMempoolTxsProgress)
          , (ElMemoryProgress,     elMemoryProgress)
          , (ElCPUProgress,        elCPUProgress)
          , (ElDiskReadProgress,   elDiskReadProgress)
          , (ElDiskWriteProgress,  elDiskWriteProgress)
          , (ElNetworkInProgress,  elNetworkInProgress)
          , (ElNetworkOutProgress, elNetworkOutProgress)
          ]

  return (body, nodeStateElems)

mainContainer :: [UI Element] -> UI Element
mainContainer elements =
  UI.div #. "w3-row main-container" #+
    [ horizontalSpacer
    , UI.div #. "w3-half w3-container w3-theme w3-border" #+ elements
    , horizontalSpacer
    ]
 where
  horizontalSpacer = UI.div #. "w3-quarter w3-container" #+ [string " "]

topNavigation :: UI Element
topNavigation =
  UI.div #. "w3-bar w3-large w3-indigo" #+
    [ UI.anchor #. "w3-bar-item" # set UI.href "https://iohk.io/" #+
        [ UI.img #. "iohk-logo" # set UI.src "/static/images/iohk-logo.png" #+ []
        ]
    , UI.div #. "w3-dropdown-hover" #+
        [ UI.button #. "w3-button" #+ [string "Node: a"]
        , UI.div #. "w3-dropdown-content w3-bar-block w3-card-4" #+
            [ UI.anchor #. "w3-bar-item w3-button" # set UI.href "#" #+ [string "a"]
            , UI.anchor #. "w3-bar-item w3-button" # set UI.href "#" #+ [string "b"]
            , UI.anchor #. "w3-bar-item w3-button" # set UI.href "#" #+ [string "c"]
            ]
        ]
    , UI.anchor #. "w3-bar-item w3-button w3-right" # set UI.href "#" #+
        [ string "Release: "
        , UI.span #. "release-name" #+ [string "Byron"]
        ]
    , UI.anchor #. "w3-bar-item w3-button w3-right" # set UI.href "#" #+
        [ string "Cardano Real-time View"
        ]
    ]

---

progressBar
  :: String
  -> String
  -> Element
  -> Element
  -> UI Element
progressBar label maxValueUnit maxValue bar = do
  UI.div #. "w3-container" #+
    [ UI.div #. "w3-row" #+
        [ UI.div #. "w3-half w3-theme" #+ [string label]
        , UI.div #. "w3-half w3-theme w3-right-align" #+
            [ element maxValue
            , UI.span #. "max-value-unit" #+ [string maxValueUnit]
            ]
        ]
    , UI.div #. "w3-border w3-light-green" #+ [element bar]
    ]

vSpacer :: String -> UI Element
vSpacer cssClassName = UI.div #. cssClassName #+ []

twoElementsInRow :: UI Element -> UI Element -> UI Element
twoElementsInRow firstOne secondOne =
  UI.div #. "w3-row" #+
    [ UI.div #. "w3-half" #+ [firstOne]
    , UI.div #. "w3-half" #+ [secondOne]
    ]

cardanoNodeCommitUrl :: String -> String
cardanoNodeCommitUrl fullCommit =
  cardanoNodeRepositoryUrl <> "commit/" <> fullCommit
 where
  cardanoNodeRepositoryUrl :: String
  cardanoNodeRepositoryUrl = "https://github.com/input-output-hk/cardano-node/"
