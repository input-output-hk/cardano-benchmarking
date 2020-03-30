{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.GUI.Markup
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

import           Cardano.Benchmarking.RTView.GUI.Elements
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
  elUptime               <- string "00:00:00"
  elEpoch                <- string "0"
  elSlot                 <- string "0"
  elBlocksNumber         <- string "0"
  elChainDensity         <- string "0"
  elTxsProcessed         <- string "0"
  elPeersNumber          <- string "0"
  elTraceAcceptorHost    <- string "0"
  elTraceAcceptorPort    <- string "0"
  elMempoolTxsNumber     <- string "0"
  elMempoolTxsPercent    <- string "0"
  elMempoolBytes         <- string "0"
  elMempoolBytesPercent  <- string "0"
  elMempoolCapacity      <- string "0"
  elMempoolCapacityBytes <- string "0"
  elMemory               <- string "0"
  elMemoryMax            <- string "0"
  elCPUMax               <- string "0"
  elCPUPercent           <- string "0"
  elDiskReadMax          <- string "0"
  elDiskReadPercent      <- string "0"
  elDiskWriteMax         <- string "0"
  elDiskWritePercent     <- string "0"
  elNetworkInMax         <- string "0"
  elNetworkInPercent     <- string "0"
  elNetworkOutMax        <- string "0"
  elNetworkOutPercent    <- string "0"

  elMempoolBytesProgress <- UI.div #. "w3-teal" #+
                              [ UI.span #. "h-spacer" #+ []
                              , element elMempoolBytes
                              , UI.span #. "percents-slash-h-spacer" #+ [string "/"]
                              , element elMempoolBytesPercent
                              , string "%"
                              ]
  elMempoolTxsProgress   <- UI.div #. "w3-teal" #+
                              [ UI.span #. "h-spacer" #+ []
                              , element elMempoolTxsNumber
                              , UI.span #. "percents-slash-h-spacer" #+ [string "/"]
                              , element elMempoolTxsPercent
                              , string "%"
                              ]
  elMemoryProgress       <- UI.div #. "w3-teal" #+
                              [ UI.span #. "h-spacer" #+ []
                              , element elMemory
                              , UI.span #. "bar-value-unit" #+ [string "MB"]
                              ]
  elCPUProgress          <- UI.div #. "w3-teal" #+
                              [ UI.span #. "h-spacer" #+ []
                              , element elCPUPercent
                              , string "%"
                              ]
  elDiskReadProgress     <- UI.div #. "w3-teal" #+
                              [ UI.span #. "h-spacer" #+ []
                              , element elDiskReadPercent
                              , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                              ]
  elDiskWriteProgress    <- UI.div #. "w3-teal" #+
                              [ UI.span #. "h-spacer" #+ []
                              , element elDiskWritePercent
                              , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                              ]
  elNetworkInProgress    <- UI.div #. "w3-teal" #+
                              [ UI.span #. "h-spacer" #+ []
                              , element elNetworkInPercent
                              , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                              ]
  elNetworkOutProgress   <- UI.div #. "w3-teal" #+
                              [ UI.span #. "h-spacer" #+ []
                              , element elNetworkOutPercent
                              , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                              ]

  body <- UI.getBody window #+
    [ topNavigation
    , mainContainer
        [ UI.div #. "w3-row main-container-inner" #+
            [ UI.div #. "w3-twothird w3-theme" #+
                [ twoElementsInRow
                    (UI.div #. "w3-container" #+
                       [ UI.div #. "w3-row" #+
                           [ UI.div #. "w3-half w3-theme" #+ [string "Mempool (Bytes)"]
                           , UI.div #. "w3-half w3-theme w3-right-align" #+
                               [ element elMempoolCapacityBytes
                               , UI.span #. "w3-tooltip" #+
                                   [ UI.span #. "info-mark" #+ [string "🛈"]
                                   , UI.span #. "w3-text w3-tag w3-indigo w3-small w3-animate-opacity w3-round-xlarge" #+
                                       [string "Capacity in bytes"]
                                   ]
                               ]
                           ]
                       , UI.div #. "w3-light-green" #+ [element elMempoolBytesProgress]
                       ])
                    (UI.div #. "w3-container" #+
                       [ UI.div #. "w3-row" #+
                           [ UI.div #. "w3-half w3-theme" #+ [string "Mempool (TXs)"]
                           , UI.div #. "w3-half w3-theme w3-right-align" #+
                               [ element elMempoolCapacity
                               , UI.span #. "w3-tooltip" #+
                                   [ UI.span #. "info-mark" #+ [string "🛈"]
                                   , UI.span #. "w3-text w3-tag w3-indigo w3-small w3-animate-opacity w3-round-xlarge" #+
                                       [string "Capacity"]
                                   ]
                               ]
                           ]
                       , UI.div #. "w3-light-green" #+ [element elMempoolTxsProgress]
                       ])
                , vSpacer "node-metrics-v-spacer"
                , progressBar "Memory usage:" " MB" elMemoryMax elMemoryProgress
                , vSpacer "node-metrics-v-spacer"
                , progressBar "CPU usage:" "%" elCPUMax elCPUProgress
                , vSpacer "node-metrics-v-spacer"
                , twoElementsInRow
                    (progressBar "Disk (RD):" " KB/s" elDiskReadMax elDiskReadProgress)
                    (progressBar "Disk (WR):" " KB/s" elDiskWriteMax elDiskWriteProgress)
                , vSpacer "node-metrics-v-spacer"
                , twoElementsInRow
                    (progressBar "Network (IN):"  " KB/s" elNetworkInMax elNetworkInProgress)
                    (progressBar "Network (OUT):" " KB/s" elNetworkOutMax elNetworkOutProgress)
                ]
            , UI.div #. "w3-third w3-container w3-theme" #+
                [ UI.div #. "w3-row" #+
                    [ UI.div #. "w3-twothird w3-container w3-theme" #+
                        [ UI.div #. "" #+
                            [ UI.div #. "" #+ [string "Node commit:"]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [string "Uptime:"]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [string "Epoch / Slot:"]
                            , UI.div #. "" #+ [string "Blocks number:"]
                            , UI.div #. "" #+ [string "Chain density:"]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [string "TXs processed:"]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [string "Peers number:"]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [string "TraceAcceptor host:"]
                            , UI.div #. "" #+ [string "TraceAcceptor port:"]
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
                            , UI.div #. "" #+
                                [ element elChainDensity
                                , UI.span #. "density-percent" #+ [string "%"]
                                ]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [element elTxsProcessed]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [element elPeersNumber]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [element elTraceAcceptorHost]
                            , UI.div #. "" #+ [element elTraceAcceptorPort]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

  -- Return these elements, they will be updated by another thread later.
  let nodeStateElems =
        Map.fromList
          [ (ElUptime,               elUptime)
          , (ElEpoch,                elEpoch)
          , (ElSlot,                 elSlot)
          , (ElBlocksNumber,         elBlocksNumber)
          , (ElChainDensity,         elChainDensity)
          , (ElTxsProcessed,         elTxsProcessed)
          , (ElPeersNumber,          elPeersNumber)
          , (ElTraceAcceptorHost,    elTraceAcceptorHost)
          , (ElTraceAcceptorPort,    elTraceAcceptorPort)
          , (ElMempoolTxsNumber,     elMempoolTxsNumber)
          , (ElMempoolTxsPercent,    elMempoolTxsPercent)
          , (ElMempoolBytes,         elMempoolBytes)
          , (ElMempoolBytesPercent,  elMempoolBytesPercent)
          , (ElMempoolCapacity,      elMempoolCapacity)
          , (ElMempoolCapacityBytes, elMempoolCapacityBytes)
          , (ElMemory,               elMemory)
          , (ElMemoryMax,            elMemoryMax)
          , (ElCPUMax,               elCPUMax)
          , (ElCPUPercent,           elCPUPercent)
          , (ElDiskReadMax,          elDiskReadMax)
          , (ElDiskReadPercent,      elDiskReadPercent)
          , (ElDiskWriteMax,         elDiskWriteMax)
          , (ElDiskWritePercent,     elDiskWritePercent)
          , (ElNetworkInMax,         elNetworkInMax)
          , (ElNetworkInPercent,     elNetworkInPercent)
          , (ElNetworkOutMax,        elNetworkOutMax)
          , (ElNetworkOutPercent,    elNetworkOutPercent)
          , (ElMempoolBytesProgress, elMempoolBytesProgress)
          , (ElMempoolTxsProgress,   elMempoolTxsProgress)
          , (ElMemoryProgress,       elMemoryProgress)
          , (ElCPUProgress,          elCPUProgress)
          , (ElDiskReadProgress,     elDiskReadProgress)
          , (ElDiskWriteProgress,    elDiskWriteProgress)
          , (ElNetworkInProgress,    elNetworkInProgress)
          , (ElNetworkOutProgress,   elNetworkOutProgress)
          ]

  return (body, nodeStateElems)

mainContainer :: [UI Element] -> UI Element
mainContainer elements =
  UI.div #. "w3-row main-container" #+
    [ horizontalSpacer
    , UI.div #. "w3-col w3-container w3-border" # set UI.style [("width", "60%")] #+ elements
    , horizontalSpacer
    ]
 where
  horizontalSpacer = UI.div #. "w3-col w3-container" # set UI.style [("width", "20%")] #+ [string " "]

topNavigation :: UI Element
topNavigation =
  UI.div #. "w3-bar w3-xlarge w3-indigo" #+
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
            , UI.span #. "value-unit" #+ [string maxValueUnit]
            ]
        ]
    , UI.div #. "w3-light-green" #+ [element bar]
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
