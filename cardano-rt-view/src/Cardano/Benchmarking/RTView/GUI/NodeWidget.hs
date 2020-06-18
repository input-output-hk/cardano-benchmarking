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
                   ( ElementName (..), NodeStateElements
                   , PeerInfoItem (..), PeerInfoElements (..)
                   )

mkNodeWidget
  :: UI (Element, NodeStateElements, [PeerInfoItem])
mkNodeWidget = do
  -- Create |Element|s containing node state (info, metrics).
  -- These elements will be part of the complete page,
  -- later they will be updated by acceptor thread.
  elNodeRelease             <- string ""
  elNodeVersion             <- string ""
  elNodePlatform            <- string ""
  elActiveNode              <- string "-"
  elUptime                  <- string "00:00:00"
  elEpoch                   <- string "0"
  elSlot                    <- string "0"
  elBlocksNumber            <- string "0"
  elBlocksForgedNumber      <- string "0"
  elNodeCannotLead          <- string "0"
  elChainDensity            <- string "0"
  elNodeIsLeaderNumber      <- string "0"
  elSlotsMissedNumber       <- string "0"
  elForksCreatedNumber      <- string "0"
  elTxsProcessed            <- string "0"
  elPeersNumber             <- string "0"
  elTraceAcceptorHost       <- string "0"
  elTraceAcceptorPort       <- string "0"
  elMempoolTxsNumber        <- string "0"
  elMempoolTxsPercent       <- string "0"
  elMempoolBytes            <- string "0"
  elMempoolBytesPercent     <- string "0"
  elMempoolMaxTxs           <- string "0"
  elMempoolMaxBytes         <- string "0"
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

  -- Progress bars.
  elMempoolBytesProgress    <- UI.div #. (show ElMempoolBytesProgress) #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elMempoolBytes
                                 , UI.span #. "percents-slash-h-spacer" #+ [string "/"]
                                 , element elMempoolBytesPercent
                                 , string "%"
                                 ]
  elMempoolBytesProgressBox <- UI.div #. (show ElMempoolBytesProgressBox) #+ [element elMempoolBytesProgress]

  elMempoolTxsProgress      <- UI.div #. (show ElMempoolTxsProgress) #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elMempoolTxsNumber
                                 , UI.span #. "percents-slash-h-spacer" #+ [string "/"]
                                 , element elMempoolTxsPercent
                                 , string "%"
                                 ]
  elMempoolTxsProgressBox   <- UI.div #. (show ElMempoolTxsProgressBox) #+ [element elMempoolTxsProgress]

  elMemoryProgress          <- UI.div #. (show ElMemoryProgress) #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elMemory
                                 , UI.span #. "bar-value-unit" #+ [string "MB"]
                                 , UI.span #. "percents-slash-h-spacer" #+ [string "/"]
                                 , UI.span #. "percents-slash-h-r-spacer" #+ [string "max"]
                                 , element elMemoryMax
                                 , UI.span #. "bar-value-unit" #+ [string "MB"]
                                 ]
  elMemoryProgressBox       <- UI.div #. (show ElMemoryProgressBox) #+ [element elMemoryProgress]

  elCPUProgress             <- UI.div #. (show ElCPUProgress) #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elCPUPercent
                                 , string "%"
                                 ]
  elCPUProgressBox          <- UI.div #. (show ElCPUProgressBox) #+ [element elCPUProgress]

  elDiskUsageRProgress      <- UI.div #. (show ElDiskReadProgress) #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elDiskUsageR
                                 , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                                 ]
  elDiskUsageRProgressBox   <- UI.div #. (show ElDiskReadProgressBox) #+ [element elDiskUsageRProgress]

  elDiskUsageWProgress      <- UI.div #. (show ElDiskWriteProgress) #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elDiskUsageW
                                 , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                                 ]
  elDiskUsageWProgressBox   <- UI.div #. (show ElDiskWriteProgressBox) #+ [element elDiskUsageWProgress]

  elNetworkUsageInProgress     <- UI.div #. (show ElNetworkInProgress) #+
                                    [ UI.span #. "h-spacer" #+ []
                                    , element elNetworkUsageIn
                                    , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                                    ]
  elNetworkUsageInProgressBox  <- UI.div #. (show ElNetworkInProgressBox) #+ [element elNetworkUsageInProgress]

  elNetworkUsageOutProgress    <- UI.div #. (show ElNetworkOutProgress) #+
                                    [ UI.span #. "h-spacer" #+ []
                                    , element elNetworkUsageOut
                                    , UI.span #. "bar-value-unit" #+ [string "KB/s"]
                                    ]
  elNetworkUsageOutProgressBox <- UI.div #. (show ElNetworkOutProgressBox) #+ [element elNetworkUsageOutProgress]

  elRTSMemoryProgress       <- UI.div #. (show ElRTSMemoryProgress) #+
                                 [ UI.span #. "h-spacer" #+ []
                                 , element elRTSMemoryUsed
                                 , UI.span #. "bar-value-unit" #+ [string "MB"]
                                 ]
  elRTSMemoryProgressBox    <- UI.div #. (show ElRTSMemoryProgressBox) #+ [element elRTSMemoryProgress]

  elNodeCommitHref <- UI.anchor # set UI.href ""
                                # set UI.target "_blank"
                                # set UI.title__ "Browse cardano-node repository on this commit"
                                #+ [string ""]

  elNodeReleaseOutdateWarning    <- infoMark "The value is outdated"
  elNodeVersionOutdateWarning    <- infoMark "The value is outdated"
  elNodePlatformOutdateWarning   <- infoMark "The value is outdated"
  elNodeCommitHrefOutdateWarning <- infoMark "The value is outdated"
  elUptimeOutdateWarning         <- infoMark "The value is outdated"

  elSlotOutdateWarning               <- infoMark "The value is outdated"
  elBlocksNumberOutdateWarning       <- infoMark "The value is outdated"
  elBlocksForgedNumberOutdateWarning <- infoMark "The value is outdated"
  elChainDensityOutdateWarning       <- infoMark "The value is outdated"
  elNodeIsLeaderNumberOutdateWarning <- infoMark "The value is outdated"
  elSlotsMissedNumberOutdateWarning  <- infoMark "The value is outdated"
  elForksCreatedNumberOutdateWarning <- infoMark "The value is outdated"

  elRTSGcCpuOutdateWarning       <- infoMark "The value is outdated"
  elRTSGcElapsedOutdateWarning   <- infoMark "The value is outdated"
  elRTSGcNumOutdateWarning       <- infoMark "The value is outdated"
  elRTSGcMajorNumOutdateWarning  <- infoMark "The value is outdated"

  -- Create content area for each tab.
  nodeTabContent
    <- UI.div #. "tab-container" # showIt #+
         [ UI.div #. "w3-row" #+
             [ UI.div #. "w3-third w3-theme" #+
                 [ UI.div #. "" #+
                     [ UI.div #. "" #+ [string "Node release:"]
                     , UI.div #. "" #+ [string "Node version:"]
                     , UI.div #. "" #+ [string "Node platform:"]
                     , UI.div #. "" #+ [string "Node commit:"]
                     , vSpacer "node-info-v-spacer"
                     , UI.div #. "" #+ [string "TraceAcceptor host:"]
                     , UI.div #. "" #+ [string "TraceAcceptor port:"]
                     , vSpacer "node-info-v-spacer"
                     , UI.div #. "" #+ [string "Node uptime:"]
                     ]
                 ]
             , UI.div #. "w3-third w3-theme" #+
                 [ UI.div #. "node-info-values" #+
                     [ UI.span #. "release-name" #+ [element elNodeRelease]
                     , UI.div #. "" #+ [element elNodeVersion]
                     , UI.div #. "" #+ [element elNodePlatform]
                     , UI.div #. "commit-link" #+ [element elNodeCommitHref]
                     , vSpacer "node-info-v-spacer"
                     , UI.div #. "" #+ [element elTraceAcceptorHost]
                     , UI.div #. "" #+ [element elTraceAcceptorPort]
                     , vSpacer "node-info-v-spacer"
                     , UI.div #. "" #+ [element elUptime]
                     ]
                 ]
             , UI.div #. "w3-third w3-theme" #+
                 [ UI.div #. "" #+
                     [ UI.span #. "" #+
                         [ element elNodeReleaseOutdateWarning
                         , UI.span # set UI.html "&nbsp;" #+ []
                         ]
                     , UI.div #. "" #+
                         [ element elNodeVersionOutdateWarning
                         , UI.span # set UI.html "&nbsp;" #+ []
                         ]
                     , UI.div #. "" #+
                         [ element elNodePlatformOutdateWarning
                         , UI.span # set UI.html "&nbsp;" #+ []
                         ]
                     , UI.div #. "" #+
                         [ element elNodeCommitHrefOutdateWarning
                         , UI.span # set UI.html "&nbsp;" #+ []
                         ]
                     , vSpacer "node-info-v-spacer"
                     , UI.div #. "" #+ [UI.span # set UI.html "&nbsp;" #+ []]
                     , UI.div #. "" #+ [UI.span # set UI.html "&nbsp;" #+ []]
                     , vSpacer "node-info-v-spacer"
                     , UI.div #. "" #+
                         [ element elUptimeOutdateWarning
                         , UI.span # set UI.html "&nbsp;" #+ []
                         ]
                     ]
                 ]
             ]
         ]

  -- List of items corresponding to each peer. To avoid dynamic changes of DOM
  -- (unfortunately, it can be a reason of space leak), we create 20 (hidden) rows
  -- corresponding to 20 connected peers. Theoretically, the number of connected
  -- peers can be bigger, but the policy of ouroboros-network is about 20 hot peers
  -- (or less).
  let supportedPeersNum = 20 :: Int -- TODO: Probably cardano-node cab trace this number?
  peersList :: [(UI Element, PeerInfoItem)]
    <- forM [1..supportedPeersNum] $ const $ do
         endpoint   <- string ""
         slotNumber <- string ""
         bytesInF   <- string ""
         reqsInF    <- string ""
         blocksInF  <- string ""
         status     <- string ""

         peerItem <- UI.div #. "w3-row" # set UI.style [("display", "none")] #+
                       [ UI.div #. "w3-col w3-theme" # set UI.style [("width", "32%")] #+ [ element endpoint ]
                       , UI.div #. "w3-col w3-theme" # set UI.style [("width", "16%")] #+ [ element slotNumber ]
                       , UI.div #. "w3-col w3-theme" # set UI.style [("width", "16%")] #+ [ element bytesInF ]
                       , UI.div #. "w3-col w3-theme" # set UI.style [("width", "10%")] #+ [ element reqsInF ]
                       , UI.div #. "w3-col w3-theme" # set UI.style [("width", "10%")] #+ [ element blocksInF ]
                       , UI.div #. "w3-col w3-theme" # set UI.style [("width", "16%")] #+ [ element status ]
                       ]
         return ( element peerItem
                , PeerInfoItem
                    peerItem
                    (PeerInfoElements endpoint bytesInF reqsInF blocksInF slotNumber status)
                )
  let (elPeersList, peerInfoItems) = unzip peersList

  peersTabContent
    <- UI.div #. "tab-container" # hideIt #+
         [ UI.div #. "w3-row" #+
             [ UI.div #. "w3-col w3-theme" # set UI.style [("width", "44%")] #+
                 [ UI.span # set UI.html "&nbsp;" #+ [] ]
             , UI.div #. "w3-rest w3-theme" #+
                 [ UI.div #. "in-flight" #+
                     [ UI.div #. "" #+ [string "In Flight"]
                     ]
                 ]
             ]
         , UI.div #. "w3-row" #+
             [ UI.div #. "w3-col w3-theme" # set UI.style [("width", "32%")] #+
                 [ UI.div #. "node-info-values" #+
                     [ UI.div #. "" #+ [string "Endpoint"]
                     ]
                 ]
             , UI.div #. "w3-col w3-theme" # set UI.style [("width", "12%")] #+
                 [ UI.div #. "node-info-values" #+
                     [ UI.div #. "" #+ [string "Slot No."]
                     ]
                 ]
             , UI.div #. "w3-col w3-theme" # set UI.style [("width", "12%")] #+
                 [ UI.div #. "in-flight-values" #+
                     [ UI.div #. "" #+ [string "Bytes"]
                     ]
                 ]
             , UI.div #. "w3-col w3-theme" # set UI.style [("width", "12%")] #+
                 [ UI.div #. "in-flight-values" #+
                     [ UI.div #. "" #+ [string "Reqs"]
                     ]
                 ]
             , UI.div #. "w3-col w3-theme" # set UI.style [("width", "12%")] #+
                 [ UI.div #. "in-flight-values" #+
                     [ UI.div #. "" #+ [string "Blocks"]
                     ]
                 ]
             , UI.div #. "w3-rest w3-theme" #+
                 [ UI.div #. "node-info-values" #+
                     [ UI.div #. "" #+ [string "Status"]
                     ]
                 ]
             ]
         , UI.div #. "" #+ elPeersList
         ]

  blockchainTabContent
    <- UI.div #. "tab-container" # hideIt #+
         [ UI.div #. "w3-row" #+
             [ UI.div #. "w3-third w3-theme" #+
                 [ UI.div #. "" #+
                     [ UI.div #. "" #+ [string "Epoch / Slot in epoch:"]
                     , UI.div #. "" #+ [string "Blocks number:"]
                     , UI.div #. "" #+ [string "Forged blocks number:"]
                     , UI.div #. "" #+ [string "Chain density:"]
                     , UI.div #. "" #+ [string "Slot leader, number:"]
                     , UI.div #. "" #+ [string "Cannot lead, number:"]
                     , UI.div #. "" #+ [string "Missed slots number:"]
                     , UI.div #. "" #+ [string "Created forks number:"]
                     ]
                 ]
             , UI.div #. "w3-third w3-theme" #+
                 [ UI.div #. "node-info-values" #+
                     [ UI.div #. "" #+
                         [ element elEpoch
                         , string " / "
                         , element elSlot
                         ]
                     , UI.div #. "" #+ [element elBlocksNumber]
                     , UI.div #. "" #+ [element elBlocksForgedNumber]
                     , UI.div #. "" #+
                         [ element elChainDensity
                         , UI.span #. "density-percent" #+ [string "%"]
                         ]
                     , UI.div #. "" #+ [element elNodeIsLeaderNumber]
                     , UI.div #. "" #+ [element elNodeCannotLead]
                     , UI.div #. "" #+ [element elSlotsMissedNumber]
                     , UI.div #. "" #+ [element elForksCreatedNumber]
                     ]
                 ]
             , UI.div #. "w3-third w3-theme" #+
                 [ UI.div #. "" #+
                     [ UI.div #. "" #+
                         [ element elSlotOutdateWarning
                         , UI.span # set UI.html "&nbsp;" #+ []
                         ]
                     , UI.div #. "" #+
                         [ element elBlocksNumberOutdateWarning
                         , UI.span # set UI.html "&nbsp;" #+ []
                         ]
                     , UI.div #. "" #+
                         [ element elBlocksForgedNumberOutdateWarning
                         , UI.span # set UI.html "&nbsp;" #+ []
                         ]
                     , UI.div #. "" #+
                         [ element elChainDensityOutdateWarning
                         , UI.span # set UI.html "&nbsp;" #+ []
                         ]
                     , UI.div #. "" #+
                         [ element elNodeIsLeaderNumberOutdateWarning
                         , UI.span # set UI.html "&nbsp;" #+ []
                         ]
                     , UI.div #. "" #+
                         [ element elSlotsMissedNumberOutdateWarning
                         , UI.span # set UI.html "&nbsp;" #+ []
                         ]
                     , UI.div #. "" #+
                         [ element elForksCreatedNumberOutdateWarning
                         , UI.span # set UI.html "&nbsp;" #+ []
                         ]
                     ]
                 ]
             ]
         ]

  mempoolTabContent
    <- UI.div #. "tab-container" # hideIt #+
         [ UI.div #. "w3-row" #+
            [ UI.div #. "" #+
                [ twoElementsInRow
                    (UI.div #. "w3-container" #+
                       [ UI.div #. "w3-row" #+
                           [ UI.div #. "w3-half w3-theme" #+ [string "Mempool (Bytes)"]
                           , UI.div #. "w3-half w3-theme w3-right-align" #+
                               [ element elMempoolMaxBytes
                               , infoMark "Maximum in bytes"
                               ]
                           ]
                       , element elMempoolBytesProgressBox
                       ])
                    (UI.div #. "w3-container" #+
                       [ UI.div #. "w3-row" #+
                           [ UI.div #. "w3-half w3-theme" #+ [string "Mempool (TXs)"]
                           , UI.div #. "w3-half w3-theme w3-right-align" #+
                               [ element elMempoolMaxTxs
                               , infoMark "Maximum in txs"
                               ]
                           ]
                       , element elMempoolTxsProgressBox
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
                    , element elMemoryProgressBox
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
                    , element elCPUProgressBox
                    ]
                , vSpacer "node-metrics-v-spacer"
                , twoElementsInRow
                    (UI.div #. "w3-container" #+
                       [ UI.div #. "w3-row" #+
                           [ UI.div #. "w3-half w3-theme" #+ [string "Disk (RD)"]
                           , UI.div #. "w3-half w3-theme w3-right-align" #+
                               [ element elDiskUsageRMaxTotal
                               , UI.span #. "value-unit" #+ [string "KB/s"]
                               , infoMark "Maximum value over the last two minutes"
                               ]
                           ]
                       , element elDiskUsageRProgressBox
                       ])
                    (UI.div #. "w3-container" #+
                       [ UI.div #. "w3-row" #+
                           [ UI.div #. "w3-half w3-theme" #+ [string "Disk (WR)"]
                           , UI.div #. "w3-half w3-theme w3-right-align" #+
                               [ element elDiskUsageWMaxTotal
                               , UI.span #. "value-unit" #+ [string "KB/s"]
                               , infoMark "Maximum value over the last two minutes"
                               ]
                           ]
                       , element elDiskUsageWProgressBox
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
                       , element elNetworkUsageInProgressBox
                       ])
                    (UI.div #. "w3-container" #+
                       [ UI.div #. "w3-row" #+
                           [ UI.div #. "w3-half w3-theme" #+ [string "Network (OUT)"]
                           , UI.div #. "w3-half w3-theme w3-right-align" #+
                               [ element elNetworkUsageOutMaxTotal
                               , UI.span #. "value-unit" #+ [string "KB/s"]
                               ]
                           ]
                       , element elNetworkUsageOutProgressBox
                       ])
                ]
            ]
         ]

  ghcRTSTabContent
    <- UI.div #. "tab-container" # hideIt #+
         [ UI.div #. "" #+
             [ UI.div #. "w3-container" #+
                 [ UI.div #. "w3-row" #+
                     [ UI.div #. "w3-half w3-theme" #+ [string "RTS live memory"]
                     , UI.div #. "w3-half w3-theme w3-right-align" #+
                         [ element elRTSMemoryAllocated
                         , UI.span #. "value-unit" #+ [string "MB"]
                         ]
                     ]
                 , element elRTSMemoryProgressBox
                 ]
             , vSpacer "node-metrics-v-spacer"
             , vSpacer "node-metrics-v-spacer"
             , UI.div #. "w3-row" #+
                 [ UI.div #. "w3-third w3-theme" #+
                     [ UI.div #. "" #+
                         [ UI.div #. "" #+ [string "GC CPU time:"]
                         , UI.div #. "" #+ [string "GC time elapsed:"]
                         , UI.div #. "" #+ [string "Number of GC runs:"]
                         , UI.div #. "" #+ [string "Major GC runs:"]
                         ]
                     ]
                 , UI.div #. "w3-third w3-theme" #+
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
                 , UI.div #. "w3-third w3-theme" #+
                     [ UI.div #. "" #+
                         [ UI.div #. "" #+
                             [ element elRTSGcCpuOutdateWarning
                             , UI.span # set UI.html "&nbsp;" #+ []
                             ]
                         , UI.div #. "" #+
                             [ element elRTSGcElapsedOutdateWarning
                             , UI.span # set UI.html "&nbsp;" #+ []
                             ]
                         , UI.div #. "" #+
                             [ element elRTSGcNumOutdateWarning
                             , UI.span # set UI.html "&nbsp;" #+ []
                             ]
                         , UI.div #. "" #+
                             [ element elRTSGcMajorNumOutdateWarning
                             , UI.span # set UI.html "&nbsp;" #+ []
                             ]
                         ]
                     ]
                 ]
             ]
         ]

  -- List of node errors, it will be changed dynamically!
  elNodeErrorsList <- UI.div #. "" #+ []

  errorsTabContent
    <- UI.div #. "tab-container errors-tab-container" # hideIt #+
         [ UI.div #. "w3-row" #+
             [ UI.div #. "w3-third w3-theme" #+
                 [ UI.div #. "node-info-values" #+
                     [ UI.div #. "" #+
                         [ string "Timestamp"
                         , infoMark "Time in UTC"
                         ]
                     ]
                 ]
             , UI.div #. "w3-twothird w3-theme" #+
                 [ UI.div #. "node-info-values" #+
                     [ UI.div #. "" #+ [string "Error message"]
                     ]
                 ]
             ]
         , element elNodeErrorsList
         ]

  -- Tabs for corresponding sections.
  nodeTab        <- UI.button #. "w3-bar-item w3-button" # makeItActive #+ [string "Node"]
  peersTab       <- UI.button #. "w3-bar-item w3-button" #+ [string "Peers"]
  blockchainTab  <- UI.button #. "w3-bar-item w3-button" #+ [string "Blockchain"]
  mempoolTab     <- UI.button #. "w3-bar-item w3-button" #+ [string "Mempool"]
  performanceTab <- UI.button #. "w3-bar-item w3-button" #+ [string "Resources"]
  ghcRTSTab      <- UI.button #. "w3-bar-item w3-button" #+ [string "GHC RTS"]
  errorsTab      <- UI.button #. "w3-bar-item w3-button" #+ [string "Errors"]

  let tabs :: [(Element, Element, String)]
      tabs =
        [ (nodeTab,        nodeTabContent,        "Node")
        , (peersTab,       peersTabContent,       "Peers")
        , (blockchainTab,  blockchainTabContent,  "Blockchain")
        , (mempoolTab,     mempoolTabContent,     "Mempool")
        , (performanceTab, performanceTabContent, "Resources")
        , (errorsTab,      errorsTabContent,      "Errors")
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
          , element mempoolTab
          , element performanceTab
          , element errorsTab
          , element ghcRTSTab
          ]
      , element nodeTabContent
      , element peersTabContent
      , element blockchainTabContent
      , element mempoolTabContent
      , element performanceTabContent
      , element errorsTabContent
      , element ghcRTSTabContent
      ]

  -- Return these elements, they will be updated by another thread later.
  let nodeStateElems =
        Map.fromList
          [ (ElNodeRelease,             elNodeRelease)
          , (ElNodeVersion,             elNodeVersion)
          , (ElNodePlatform,            elNodePlatform)
          , (ElNodeCommitHref,          elNodeCommitHref)
          , (ElActiveNode,              elActiveNode)
          , (ElUptime,                  elUptime)
          , (ElEpoch,                   elEpoch)
          , (ElSlot,                    elSlot)
          , (ElBlocksNumber,            elBlocksNumber)
          , (ElBlocksForgedNumber,      elBlocksForgedNumber)
          , (ElNodeCannotLead,          elNodeCannotLead)
          , (ElChainDensity,            elChainDensity)
          , (ElNodeIsLeaderNumber,      elNodeIsLeaderNumber)
          , (ElSlotsMissedNumber,       elSlotsMissedNumber)
          , (ElForksCreatedNumber,      elForksCreatedNumber)
          , (ElTxsProcessed,            elTxsProcessed)
          , (ElPeersNumber,             elPeersNumber)
          , (ElTraceAcceptorHost,       elTraceAcceptorHost)
          , (ElTraceAcceptorPort,       elTraceAcceptorPort)
          , (ElNodeErrors,              elNodeErrorsList)
          , (ElMempoolTxsNumber,        elMempoolTxsNumber)
          , (ElMempoolTxsPercent,       elMempoolTxsPercent)
          , (ElMempoolBytes,            elMempoolBytes)
          , (ElMempoolBytesPercent,     elMempoolBytesPercent)
          , (ElMempoolMaxTxs,           elMempoolMaxTxs)
          , (ElMempoolMaxBytes,         elMempoolMaxBytes)
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
          -- Outdated warnings
          , (ElNodeReleaseOutdateWarning,        elNodeReleaseOutdateWarning)
          , (ElNodeVersionOutdateWarning,        elNodeVersionOutdateWarning)
          , (ElNodePlatformOutdateWarning,       elNodePlatformOutdateWarning)
          , (ElNodeCommitHrefOutdateWarning,     elNodeCommitHrefOutdateWarning)
          , (ElUptimeOutdateWarning,             elUptimeOutdateWarning)
          , (ElSlotOutdateWarning,               elSlotOutdateWarning)
          , (ElBlocksNumberOutdateWarning,       elBlocksNumberOutdateWarning)
          , (ElBlocksForgedNumberOutdateWarning, elBlocksForgedNumberOutdateWarning)
          , (ElChainDensityOutdateWarning,       elChainDensityOutdateWarning)
          , (ElNodeIsLeaderNumberOutdateWarning, elNodeIsLeaderNumberOutdateWarning)
          , (ElSlotsMissedNumberOutdateWarning,  elSlotsMissedNumberOutdateWarning)
          , (ElForksCreatedNumberOutdateWarning, elForksCreatedNumberOutdateWarning)
          , (ElRTSGcCpuOutdateWarning,           elRTSGcCpuOutdateWarning)
          , (ElRTSGcElapsedOutdateWarning,       elRTSGcElapsedOutdateWarning)
          , (ElRTSGcNumOutdateWarning,           elRTSGcNumOutdateWarning)
          , (ElRTSGcMajorNumOutdateWarning,      elRTSGcMajorNumOutdateWarning)
          -- Progress bars
          , (ElMempoolBytesProgress,    elMempoolBytesProgress)
          , (ElMempoolBytesProgressBox, elMempoolBytesProgressBox)
          , (ElMempoolTxsProgress,      elMempoolTxsProgress)
          , (ElMempoolTxsProgressBox,   elMempoolTxsProgressBox)
          , (ElMemoryProgress,          elMemoryProgress)
          , (ElMemoryProgressBox,       elMemoryProgressBox)
          , (ElCPUProgress,             elCPUProgress)
          , (ElCPUProgressBox,          elCPUProgressBox)
          , (ElDiskReadProgress,        elDiskUsageRProgress)
          , (ElDiskReadProgressBox,     elDiskUsageRProgressBox)
          , (ElDiskWriteProgress,       elDiskUsageWProgress)
          , (ElDiskWriteProgressBox,    elDiskUsageWProgressBox)
          , (ElNetworkInProgress,       elNetworkUsageInProgress)
          , (ElNetworkInProgressBox,    elNetworkUsageInProgressBox)
          , (ElNetworkOutProgress,      elNetworkUsageOutProgress)
          , (ElNetworkOutProgressBox,   elNetworkUsageOutProgressBox)
          , (ElRTSMemoryProgress,       elRTSMemoryProgress)
          , (ElRTSMemoryProgressBox,    elRTSMemoryProgressBox)
          ]

  return (nodeWidget, nodeStateElems, peerInfoItems)

---

vSpacer :: String -> UI Element
vSpacer cssClassName = UI.div #. cssClassName #+ []

twoElementsInRow :: UI Element -> UI Element -> UI Element
twoElementsInRow firstOne secondOne =
  UI.div #. "w3-row" #+
    [ UI.div #. "w3-half" #+ [firstOne]
    , UI.div #. "w3-half" #+ [secondOne]
    ]

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

infoMark :: String -> UI Element
infoMark aTitle =
  UI.span #. "info-mark"
          #  set UI.title__ aTitle
          #+ [string "🛈"]
