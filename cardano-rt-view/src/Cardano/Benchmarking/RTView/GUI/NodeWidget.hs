{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.GUI.NodeWidget
    ( mkNodeWidget
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

mkNodeWidget
  :: UI (Element, NodeStateElements)
mkNodeWidget = do
  -- Create |Element|s containing node state (info, metrics).
  -- These elements will be part of the complete page,
  -- later they will be updated by acceptor thread.
  elNodeRelease             <- string ""
  elNodeVersion             <- string ""
  elNodeCommit              <- string ""
  elNodeShortCommit         <- string ""
  elActiveNode              <- string "-"
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
  elRTSMemoryAllocated      <- string "0"
  elRTSMemoryUsed           <- string "0"
  elRTSMemoryUsedPercent    <- string "0"
  elRTSGcCpu                <- string "0"
  elRTSGcElapsed            <- string "0"
  elRTSGcNum                <- string "0"
  elRTSGcMajorNum           <- string "0"

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
  elCPUProgress             <- UI.div #. "w3-indigo" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elCPUPercent
                                 , string "%"
                                 ]
  elDiskUsageRProgress      <- UI.div #. "w3-deep-orange" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elDiskUsageR
                                 , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                                 ]
  elDiskUsageWProgress      <- UI.div #. "w3-deep-orange" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elDiskUsageW
                                 , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                                 ]
  elNetworkUsageInProgress  <- UI.div #. "w3-purple" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elNetworkUsageIn
                                 , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                                 ]
  elNetworkUsageOutProgress <- UI.div #. "w3-purple" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elNetworkUsageOut
                                 , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                                 ]

  elRTSMemoryProgress       <- UI.div #. "w3-teal" #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elRTSMemoryUsed
                                 , UI.span #. "bar-value-unit" #+ [string "MB"]
                                 ]

  -- TODO: Currently there's no real cardano-node commit as a string.
  -- It will be taken from MVar NodesState later, when connected nodes selector
  -- will be implemented.
  let fullCommit :: String
      fullCommit = "#"

  -- Create content area for each tab.
  nodeTabContent
    <- UI.div #. "tab-container" # showIt #+
         [ UI.div #. "w3-row" #+
             [ UI.div #. "w3-half w3-theme" #+
                 [ UI.div #. "" #+
                     [ UI.div #. "" #+ [string "Node release:"]
                     , UI.div #. "" #+ [string "Node version:"]
                     , UI.div #. "" #+ [string "Node commit:"]
                     , vSpacer "node-info-v-spacer"
                     , UI.div #. "" #+ [string "TraceAcceptor host:"]
                     , UI.div #. "" #+ [string "TraceAcceptor port:"]
                     , vSpacer "node-info-v-spacer"
                     , UI.div #. "" #+ [string "Node uptime:"]
                     ]
                 ]
             , UI.div #. "w3-half w3-theme" #+
                 [ UI.div #. "node-info-values" #+
                     [ UI.span #. "release-name" #+ [element elNodeRelease]
                     , UI.div #. "" #+ [element elNodeVersion]
                     , UI.div #. "commit-link" #+
                         [ UI.anchor # set UI.href (cardanoNodeCommitUrl fullCommit)
                                     # set UI.target "_blank"
                                     # set UI.title__ "Browse cardano-node repository on this commit"
                                     #+ [element elNodeShortCommit]
                         ]
                     , vSpacer "node-info-v-spacer"
                     , UI.div #. "" #+ [element elTraceAcceptorHost]
                     , UI.div #. "" #+ [element elTraceAcceptorPort]
                     , vSpacer "node-info-v-spacer"
                     , UI.div #. "" #+ [element elUptime]
                     ]
                 ]
             ]
         ]

  -- List of items corresponding to each peer, it will be changed dynamically!
  elPeersList <- UI.div #. "" #+ []

  peersTabContent
    <- UI.div #. "tab-container" # hideIt #+
         [ UI.div #. "w3-row" #+
             [ UI.div #. "w3-third w3-theme" #+
                 [ UI.div #. "node-info-values" #+
                     [ UI.div #. "" #+ [string "Endpoint"]
                     ]
                 ]
             , UI.div #. "w3-third w3-theme" #+
                 [ UI.div #. "node-info-values" #+
                     [ UI.div #. "" #+ [string "Slot No."]
                     ]
                 ]
             , UI.div #. "w3-third w3-theme" #+
                 [ UI.div #. "node-info-values" #+
                     [ UI.div #. "" #+ [string "Block No."]
                     ]
                 ]
             ]
         , element elPeersList
         ]

  blockchainTabContent
    <- UI.div #. "tab-container" # hideIt #+
         [ UI.div #. "w3-row" #+
             [ UI.div #. "w3-half w3-theme" #+
                 [ UI.div #. "" #+
                     [ UI.div #. "" #+ [string "Epoch / Slot in epoch:"]
                     , UI.div #. "" #+ [string "Blocks number:"]
                     , UI.div #. "" #+ [string "Chain density:"]
                     ]
                 ]
             , UI.div #. "w3-half w3-theme" #+
                 [ UI.div #. "node-info-values" #+
                     [ UI.div #. "" #+
                         [ element elEpoch
                         , string " / "
                         , element elSlot
                         ]
                     , UI.div #. "" #+ [element elBlocksNumber]
                     , UI.div #. "" #+
                         [ element elChainDensity
                         , UI.span #. "density-percent" #+ [string "%"]
                         ]
                     ]
                 ]
             ]
         ]

  txTabContent
    <- UI.div #. "tab-container" # hideIt #+
         [ UI.div #. "w3-row" #+
            [ UI.div #. "" #+
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
                , vSpacer "node-metrics-v-spacer"
                , UI.div #. "w3-row" #+
                     [ UI.div #. "w3-half w3-theme" #+
                         [ UI.div #. "" #+
                             [ UI.div #. "" #+ [string "TXs processed:"]
                             ]
                         ]
                     , UI.div #. "w3-half w3-theme" #+
                         [ UI.div #. "node-info-values" #+
                             [ UI.div #. "" #+ [element elTxsProcessed]
                             ]
                         ]
                     ]
                ]
            ]
         ]

  performanceTabContent
    <- UI.div #. "tab-container" # hideIt #+
         [ UI.div #. "w3-row" #+
            [ UI.div #. "" #+
                [ UI.div #. "w3-container" #+
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
                    , UI.div #. "w3-cyan" #+ [element elCPUProgress]
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
                       , UI.div #. "w3-amber" #+ [element elDiskUsageRProgress]
                       ])
                    (UI.div #. "w3-container" #+
                       [ UI.div #. "w3-row" #+
                           [ UI.div #. "w3-half w3-theme" #+ [string "Disk (WR)"]
                           , UI.div #. "w3-half w3-theme w3-right-align" #+
                               [ element elDiskUsageWMaxTotal
                               , UI.span #. "value-unit" #+ [string "KB/s"]
                               ]
                           ]
                       , UI.div #. "w3-amber" #+ [element elDiskUsageWProgress]
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
                       , UI.div #. "w3-pale-red" #+ [element elNetworkUsageInProgress]
                       ])
                    (UI.div #. "w3-container" #+
                       [ UI.div #. "w3-row" #+
                           [ UI.div #. "w3-half w3-theme" #+ [string "Network (OUT)"]
                           , UI.div #. "w3-half w3-theme w3-right-align" #+
                               [ element elNetworkUsageOutMaxTotal
                               , UI.span #. "value-unit" #+ [string "KB/s"]
                               ]
                           ]
                       , UI.div #. "w3-pale-red" #+ [element elNetworkUsageOutProgress]
                       ])
                ]
            ]
         ]

  ghcRTSTabContent
    <- UI.div #. "tab-container" # hideIt #+
         [ UI.div #. "" #+
             [ UI.div #. "w3-container" #+
                 [ UI.div #. "w3-row" #+
                     [ UI.div #. "w3-half w3-theme" #+ [string "RTS memory"]
                     , UI.div #. "w3-half w3-theme w3-right-align" #+
                         [ element elRTSMemoryAllocated
                         , UI.span #. "value-unit" #+ [string "MB"]
                         ]
                     ]
                 , UI.div #. "w3-light-green" #+ [element elRTSMemoryProgress]
                 ]
             , vSpacer "node-metrics-v-spacer"
             , vSpacer "node-metrics-v-spacer"
             , UI.div #. "w3-row" #+
                 [ UI.div #. "w3-half w3-theme" #+
                     [ UI.div #. "" #+
                         [ UI.div #. "" #+ [string "GC CPU:"]
                         , UI.div #. "" #+ [string "GC Elapsed:"]
                         , UI.div #. "" #+ [string "GC Number:"]
                         , UI.div #. "" #+ [string "GC Major Number:"]
                         ]
                     ]
                 , UI.div #. "w3-half w3-theme" #+
                     [ UI.div #. "node-info-values" #+
                         [ UI.div #. "" #+
                             [ element elRTSGcCpu
                             , UI.span #. "value-unit" #+ [string "s"]
                             ]
                         , UI.div #. "" #+
                             [ element elRTSGcElapsed
                             , UI.span #. "value-unit" #+ [string "s"]
                             ]
                         , UI.div #. "" #+ [element elRTSGcNum]
                         , UI.div #. "" #+ [element elRTSGcMajorNum]
                         ]
                     ]
                 ]
             ]
         ]

  -- Tabs for corresponding sections.
  nodeTab        <- UI.button #. "w3-bar-item w3-button" # makeItActive #+ [string "Node"]
  peersTab       <- UI.button #. "w3-bar-item w3-button" #+ [string "Peers"]
  blockchainTab  <- UI.button #. "w3-bar-item w3-button" #+ [string "Blockchain"]
  txTab          <- UI.button #. "w3-bar-item w3-button" #+ [string "TXs"]
  performanceTab <- UI.button #. "w3-bar-item w3-button" #+ [string "Performance"]
  ghcRTSTab      <- UI.button #. "w3-bar-item w3-button" #+ [string "GHC RTS"]

  let tabs :: [(Element, Element, String)]
      tabs =
        [ (nodeTab,        nodeTabContent,        "Node")
        , (peersTab,       peersTabContent,       "Peers")
        , (blockchainTab,  blockchainTabContent,  "Blockchain")
        , (txTab,          txTabContent,          "TXs")
        , (performanceTab, performanceTabContent, "Performance")
        , (ghcRTSTab,      ghcRTSTabContent,      "GHC RTS")
        ]

  registerClicksOnTabs tabs

  -- Make a widget for one node.
  nodeWidget <-
    UI.div #. "w3-container w3-margin w3-border node-container" #+
      [ UI.div #. "node-name" #+ [element elActiveNode]
      , UI.div #. "w3-bar w3-blue-gray" #+
          [ element nodeTab
          , element peersTab
          , element blockchainTab
          , element txTab
          , element performanceTab
          , element ghcRTSTab
          ]
      , element nodeTabContent
      , element peersTabContent
      , element blockchainTabContent
      , element txTabContent
      , element performanceTabContent
      , element ghcRTSTabContent
      ]

  -- Return these elements, they will be updated by another thread later.
  let nodeStateElems =
        Map.fromList
          [ (ElNodeRelease,             elNodeRelease)
          , (ElNodeVersion,             elNodeVersion)
          , (ElNodeCommit,              elNodeCommit)
          , (ElNodeShortCommit,         elNodeShortCommit)
          , (ElActiveNode,              elActiveNode)
          , (ElUptime,                  elUptime)
          , (ElEpoch,                   elEpoch)
          , (ElSlot,                    elSlot)
          , (ElBlocksNumber,            elBlocksNumber)
          , (ElChainDensity,            elChainDensity)
          , (ElTxsProcessed,            elTxsProcessed)
          , (ElPeersNumber,             elPeersNumber)
          , (ElPeersList,               elPeersList)
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
          , (ElRTSMemoryAllocated,      elRTSMemoryAllocated)
          , (ElRTSMemoryUsed,           elRTSMemoryUsed)
          , (ElRTSMemoryUsedPercent,    elRTSMemoryUsedPercent)
          , (ElRTSGcCpu,                elRTSGcCpu)
          , (ElRTSGcElapsed,            elRTSGcElapsed)
          , (ElRTSGcNum,                elRTSGcNum)
          , (ElRTSGcMajorNum,           elRTSGcMajorNum)
          -- Progress bars
          , (ElMempoolBytesProgress,    elMempoolBytesProgress)
          , (ElMempoolTxsProgress,      elMempoolTxsProgress)
          , (ElMemoryProgress,          elMemoryProgress)
          , (ElCPUProgress,             elCPUProgress)
          , (ElDiskReadProgress,        elDiskUsageRProgress)
          , (ElDiskWriteProgress,       elDiskUsageWProgress)
          , (ElNetworkInProgress,       elNetworkUsageInProgress)
          , (ElNetworkOutProgress,      elNetworkUsageOutProgress)
          , (ElRTSMemoryProgress,       elRTSMemoryProgress)
          ]

  return (nodeWidget, nodeStateElems)

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

-- | Since information and metrics are splitted to tabs,
--   we have to make them clickable and show which one is active.
registerClicksOnTabs
  :: [(Element, Element, String)]
  -> UI ()
registerClicksOnTabs tabs =
  forM_ tabs $ \(tab, _, tabName) ->
    void $ UI.onEvent (UI.click tab) $ \_ -> showTabAndMakeItActive tabName
 where
  showTabAndMakeItActive aName =
    forM_ tabs $ \(tab', tabContent, tabName') ->
      if aName == tabName'
        then do
          void $ element tabContent # showIt
          void $ element tab' # makeItActive
        else do
          void $ element tabContent # hideIt
          void $ element tab' # makeItInactive

showIt, hideIt :: UI Element -> UI Element
showIt = set UI.style [("display", "block")]
hideIt = set UI.style [("display", "none")]

makeItActive, makeItInactive :: UI Element -> UI Element
makeItActive   = set UI.class_ "w3-bar-item w3-button w3-khaki"
makeItInactive = set UI.class_ "w3-bar-item w3-button"
