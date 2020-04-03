{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.GUI.Markup
    ( mkPageBody
    ) where

import           Cardano.Prelude
import           Prelude
                   ( String )
import qualified Data.Map.Strict as Map

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
  -- Create |Element|s containing node state (info, metrics).
  -- These elements will be part of the complete page,
  -- later they will be updated by acceptor thread.
  elNodeRelease             <- string ""
  elNodeVersion             <- string ""
  elNodeCommit              <- string ""
  elNodeShortCommit         <- string ""
  elUptime                  <- string "00:00:00"
  elEpoch                   <- string "0"
  elSlot                    <- string "0"
  elBlocksNumber            <- string "0"
  elChainDensity            <- string "0"
  elTxsProcessed            <- string "0"
  elPeersNumber             <- string "0"
  elTraceAcceptorHost       <- string "0"
  elTraceAcceptorPort       <- string "0"
  elMempoolTxsNumber        <- string "0"
  elMempoolTxsPercent       <- string "0"
  elMempoolBytes            <- string "0"
  elMempoolBytesPercent     <- string "0"
  elMempoolCapacity         <- string "0"
  elMempoolCapacityBytes    <- string "0"
  elMemory                  <- string "0"
  elMemoryMax               <- string "0"
  elMemoryMaxTotal          <- string "0"
  elMemoryPercent           <- string "0"
  elCPUPercent              <- string "0"
  elDiskUsageR              <- string "0"
  elDiskUsageRMaxTotal      <- string "0"
  elDiskUsageW              <- string "0"
  elDiskUsageWMaxTotal      <- string "0"
  elNetworkUsageIn          <- string "0"
  elNetworkUsageInMaxTotal  <- string "0"
  elNetworkUsageOut         <- string "0"
  elNetworkUsageOutMaxTotal <- string "0"

  elMempoolBytesProgress    <- UI.div #. "w3-teal" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elMempoolBytes
                                 , UI.span #. "percents-slash-h-spacer" #+ [string "/"]
                                 , element elMempoolBytesPercent
                                 , string "%"
                                 ]
  elMempoolTxsProgress      <- UI.div #. "w3-teal" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elMempoolTxsNumber
                                 , UI.span #. "percents-slash-h-spacer" #+ [string "/"]
                                 , element elMempoolTxsPercent
                                 , string "%"
                                 ]
  elMemoryProgress          <- UI.div #. "w3-teal" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elMemory
                                 , UI.span #. "bar-value-unit" #+ [string "MB"]
                                 , UI.span #. "percents-slash-h-spacer" #+ [string "/"]
                                 , UI.span #. "percents-slash-h-r-spacer" #+ [string "max"]
                                 , element elMemoryMax
                                 , UI.span #. "bar-value-unit" #+ [string "MB"]
                                 ]
  elCPUProgress             <- UI.div #. "w3-teal" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elCPUPercent
                                 , string "%"
                                 ]
  elDiskUsageRProgress      <- UI.div #. "w3-teal" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elDiskUsageR
                                 , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                                 ]
  elDiskUsageWProgress      <- UI.div #. "w3-teal" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elDiskUsageW
                                 , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                                 ]
  elNetworkUsageInProgress  <- UI.div #. "w3-teal" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elNetworkUsageIn
                                 , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                                 ]
  elNetworkUsageOutProgress <- UI.div #. "w3-teal" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elNetworkUsageOut
                                 , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                                 ]

  -- TODO: Currently there's no real cardano-node commit as a string.
  -- It will be taken from MVar NodesState later, when connected nodes selector
  -- will be implemented.
  let fullCommit :: String
      fullCommit = "#"

  body <- UI.getBody window #+
    [ topNavigation elNodeRelease
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
                , UI.div #. "w3-container" #+
                    [ UI.div #. "w3-row" #+
                        [ UI.div #. "w3-half w3-theme" #+ [string "Memory usage"]
                        , UI.div #. "w3-half w3-theme w3-right-align" #+
                            [ element elMemoryMaxTotal
                            , UI.span #. "value-unit" #+ [string "MB"]
                            ]
                        ]
                    , UI.div #. "w3-light-green" #+ [element elMemoryProgress]
                    ]
                , vSpacer "node-metrics-v-spacer"
                , UI.div #. "w3-container" #+
                    [ UI.div #. "w3-row" #+
                        [ UI.div #. "w3-half w3-theme" #+ [string "CPU usage"]
                        , UI.div #. "w3-half w3-theme w3-right-align" #+
                            [ string "100"
                            , UI.span #. "value-unit" #+ [string "%"]
                            ]
                        ]
                    , UI.div #. "w3-light-green" #+ [element elCPUProgress]
                    ]
                , vSpacer "node-metrics-v-spacer"
                , twoElementsInRow
                    (UI.div #. "w3-container" #+
                       [ UI.div #. "w3-row" #+
                           [ UI.div #. "w3-half w3-theme" #+ [string "Disk (RD)"]
                           , UI.div #. "w3-half w3-theme w3-right-align" #+
                               [ element elDiskUsageRMaxTotal
                               , UI.span #. "value-unit" #+ [string "KB/s"]
                               ]
                           ]
                       , UI.div #. "w3-light-green" #+ [element elDiskUsageRProgress]
                       ])
                    (UI.div #. "w3-container" #+
                       [ UI.div #. "w3-row" #+
                           [ UI.div #. "w3-half w3-theme" #+ [string "Disk (WR)"]
                           , UI.div #. "w3-half w3-theme w3-right-align" #+
                               [ element elDiskUsageWMaxTotal
                               , UI.span #. "value-unit" #+ [string "KB/s"]
                               ]
                           ]
                       , UI.div #. "w3-light-green" #+ [element elDiskUsageWProgress]
                       ])
                , vSpacer "node-metrics-v-spacer"
                , twoElementsInRow
                    (UI.div #. "w3-container" #+
                       [ UI.div #. "w3-row" #+
                           [ UI.div #. "w3-half w3-theme" #+ [string "Network (IN)"]
                           , UI.div #. "w3-half w3-theme w3-right-align" #+
                               [ element elNetworkUsageInMaxTotal
                               , UI.span #. "value-unit" #+ [string "KB/s"]
                               ]
                           ]
                       , UI.div #. "w3-light-green" #+ [element elNetworkUsageInProgress]
                       ])
                    (UI.div #. "w3-container" #+
                       [ UI.div #. "w3-row" #+
                           [ UI.div #. "w3-half w3-theme" #+ [string "Network (OUT)"]
                           , UI.div #. "w3-half w3-theme w3-right-align" #+
                               [ element elNetworkUsageOutMaxTotal
                               , UI.span #. "value-unit" #+ [string "KB/s"]
                               ]
                           ]
                       , UI.div #. "w3-light-green" #+ [element elNetworkUsageOutProgress]
                       ])
                ]
            , UI.div #. "w3-third w3-container w3-theme" #+
                [ UI.div #. "w3-row" #+
                    [ UI.div #. "w3-twothird w3-container w3-theme" #+
                        [ UI.div #. "" #+
                            [ UI.div #. "" #+ [string "Node version:"]
                            , UI.div #. "" #+ [string "Node commit:"]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [string "Uptime:"]
                            , vSpacer "node-info-v-spacer"
                            , UI.div #. "" #+ [string "Epoch / Slot in epoch:"]
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
                            [ UI.div #. "" #+ [element elNodeVersion]
                            , UI.div #. "commit-link" #+
                                [ UI.anchor # set UI.href (cardanoNodeCommitUrl fullCommit)
                                            # set UI.target "_blank"
                                            # set UI.title__ "Browse cardano-node repository on this commit"
                                            #+ [element elNodeShortCommit]
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
          [ (ElNodeRelease,             elNodeRelease)
          , (ElNodeVersion,             elNodeVersion)
          , (ElNodeCommit,              elNodeCommit)
          , (ElNodeShortCommit,         elNodeShortCommit)
          , (ElUptime,                  elUptime)
          , (ElEpoch,                   elEpoch)
          , (ElSlot,                    elSlot)
          , (ElBlocksNumber,            elBlocksNumber)
          , (ElChainDensity,            elChainDensity)
          , (ElTxsProcessed,            elTxsProcessed)
          , (ElPeersNumber,             elPeersNumber)
          , (ElTraceAcceptorHost,       elTraceAcceptorHost)
          , (ElTraceAcceptorPort,       elTraceAcceptorPort)
          , (ElMempoolTxsNumber,        elMempoolTxsNumber)
          , (ElMempoolTxsPercent,       elMempoolTxsPercent)
          , (ElMempoolBytes,            elMempoolBytes)
          , (ElMempoolBytesPercent,     elMempoolBytesPercent)
          , (ElMempoolCapacity,         elMempoolCapacity)
          , (ElMempoolCapacityBytes,    elMempoolCapacityBytes)
          , (ElMemory,                  elMemory)
          , (ElMemoryMax,               elMemoryMax)
          , (ElMemoryMaxTotal,          elMemoryMaxTotal)
          , (ElMemoryPercent,           elMemoryPercent)
          , (ElCPUPercent,              elCPUPercent)
          , (ElDiskUsageR,              elDiskUsageR)
          , (ElDiskUsageRMaxTotal,      elDiskUsageRMaxTotal)
          , (ElDiskUsageW,              elDiskUsageW)
          , (ElDiskUsageWMaxTotal,      elDiskUsageWMaxTotal)
          , (ElNetworkUsageIn,          elNetworkUsageIn)
          , (ElNetworkUsageInMaxTotal,  elNetworkUsageInMaxTotal)
          , (ElNetworkUsageOut,         elNetworkUsageOut)
          , (ElNetworkUsageOutMaxTotal, elNetworkUsageOutMaxTotal)
          -- Progress bars
          , (ElMempoolBytesProgress,    elMempoolBytesProgress)
          , (ElMempoolTxsProgress,      elMempoolTxsProgress)
          , (ElMemoryProgress,          elMemoryProgress)
          , (ElCPUProgress,             elCPUProgress)
          , (ElDiskReadProgress,        elDiskUsageRProgress)
          , (ElDiskWriteProgress,       elDiskUsageWProgress)
          , (ElNetworkInProgress,       elNetworkUsageInProgress)
          , (ElNetworkOutProgress,      elNetworkUsageOutProgress)
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

topNavigation :: Element -> UI Element
topNavigation nodeRelease =
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
        , UI.span #. "release-name" #+ [element nodeRelease]
        ]
    , UI.anchor #. "w3-bar-item w3-button w3-right" # set UI.href "#" #+
        [ string "Cardano Real-time View"
        ]
    ]

---

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
