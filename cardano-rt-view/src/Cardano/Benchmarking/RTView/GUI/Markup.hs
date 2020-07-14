{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.GUI.Markup
    ( mkPageBody
    ) where

import           Cardano.Prelude
import           Prelude
                   ( String )

import qualified Data.Text as T

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
                   ( Element, UI
                   , (#), (#+), (#.)
                   , element, set, string
                   )

import           Cardano.BM.Data.Configuration
                   ( RemoteAddrNamed (..) )
import qualified Cardano.Benchmarking.RTView.GUI.Charts as Chart
import           Cardano.Benchmarking.RTView.GUI.Elements
                   ( ElementName (..), NodeStateElements
                   , NodesStateElements, PeerInfoItem
                   )
import           Cardano.Benchmarking.RTView.GUI.Grid
                   ( allMetricsNames, mkNodesGrid, metricLabel )
import           Cardano.Benchmarking.RTView.GUI.NodeWidget
                   ( mkNodeWidget )

mkPageBody
  :: UI.Window
  -> [RemoteAddrNamed]
  -> UI ( Element
        , (NodesStateElements, NodesStateElements)
        )
mkPageBody window acceptors = do
  -- Create widgets for each node (corresponding to acceptors).
  nodeWidgetsWithElems
    <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
         (widget, nodeStateElems, peerInfoItems) <- mkNodeWidget nameOfNode
         return (nameOfNode, widget, nodeStateElems, peerInfoItems)

  -- Create widgets areas on the page.
  widgetsAreas
    <- forM nodeWidgetsWithElems $ \(_, widget, _, _) ->
         return $ UI.div #. "w3-col l6 m12 s12" #+ [element widget]

  -- Register clickable selector for nodes (to be able to show only one or all of them).
  nodesSelector <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
    nodeCheckbox
      <- UI.input #. "w3-check select-node-check"
                  # set UI.type_ "checkbox"
                  # set UI.checked True
                  #+ []
    nodeButton <-
      UI.div #. "select-node-check-area" #+
        [ element nodeCheckbox
        , UI.label #+ [UI.string $ T.unpack nameOfNode]
        ]
    void $ UI.onEvent (UI.checkedChange nodeCheckbox) $ \isChecked -> do
      UI.getElementById window "viewModeButton" >>= \case
        Just btn -> UI.get UI.value btn >>= \case
          "paneMode" -> do
            let action = if isChecked then showIt else hideIt
            forNode nameOfNode nodeWidgetsWithElems action
          _ -> do
            let action = if isChecked then showCell else hideIt
            forNodeColumn window nameOfNode action

        Nothing -> return ()
      changeStatusOfShowAllButton window "showAllNodesButton" "select-node-check"
    return $ element nodeButton

  showAllNodesButton
    <- if length nodesSelector > 1
         then do
           allNodesButton <- UI.anchor #. "w3-bar-item w3-button w3-border-top w3-disabled"
                                       # set UI.id_ "showAllNodesButton"
                                       # set UI.href "#"
                                       #+ [UI.string "Show all"]
           void $ UI.onEvent (UI.click allNodesButton) $ \_ -> do
             UI.getElementById window "viewModeButton" >>= \case
               Just btn -> UI.get UI.value btn >>= \case
                 "paneMode" -> showAllNodes window nodeWidgetsWithElems
                 _ -> showAllNodesColumns window nodeWidgetsWithElems
               Nothing -> return ()
             -- All nodes checkboxes are already shown, disable button again.
             void $ element allNodesButton # set UI.class_ "w3-bar-item w3-button w3-border-top w3-disabled"
           return [element allNodesButton]
         else
           return []

  let allSelectors = nodesSelector ++ showAllNodesButton

  -- View mode buttons.
  paneViewButton <- UI.anchor #. "w3-bar-item w3-button" # set UI.href "#" #+ [UI.string "Pane view"]
  gridViewButton <- UI.anchor #. "w3-bar-item w3-button" # set UI.href "#" #+ [UI.string "Grid view"]
  let viewModeSelector :: [UI Element]
      viewModeSelector = [ element paneViewButton
                         , element gridViewButton
                         ]

  metricsSelector <- mkMetricsSelector window

  -- Make page body.
  (gridNodes, gridNodesStateElems) <- mkNodesGrid window acceptors
  widgets <- UI.div #. "w3-row" #+ widgetsAreas
  body
    <- UI.getBody window #+
         [ topNavigation allSelectors viewModeSelector metricsSelector
         , element widgets
         ]

  UI.runFunction $ UI.ffi Chart.prepareChartsJS

  void $ UI.onEvent (UI.click paneViewButton) $ \_ -> do
    toggleViewMode window "paneMode" widgets widgetsAreas [element gridNodes]
    forElementWithId window "selectMetricButton" hideIt
  void $ UI.onEvent (UI.click gridViewButton) $ \_ -> do
    toggleViewMode window "gridMode" widgets [element gridNodes] widgetsAreas
    forElementWithId window "selectMetricButton" showIt
    forM_ acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
      UI.runFunction $ UI.ffi Chart.gridMemoryUsageChartJS  ("grid-memoryUsageChart-"  <> nameOfNode)
      UI.runFunction $ UI.ffi Chart.gridCPUUsageChartJS     ("grid-cpuUsageChart-"     <> nameOfNode)
      UI.runFunction $ UI.ffi Chart.gridDiskUsageChartJS    ("grid-diskUsageChart-"    <> nameOfNode)
      UI.runFunction $ UI.ffi Chart.gridNetworkUsageChartJS ("grid-networkUsageChart-" <> nameOfNode)

  forM_ acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
    -- Charts for different metrics.
    UI.runFunction $ UI.ffi Chart.memoryUsageChartJS  ("memoryUsageChart-"  <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.cpuUsageChartJS     ("cpuUsageChart-"     <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.diskUsageChartJS    ("diskUsageChart-"    <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.networkUsageChartJS ("networkUsageChart-" <> nameOfNode)

  nodesStateElems
    <- forM nodeWidgetsWithElems $ \(nameOfNode, _, nodeStateElems, peerInfoItems) ->
         return (nameOfNode, nodeStateElems, peerInfoItems)

  return (body, (nodesStateElems, gridNodesStateElems))

topNavigation
  :: [UI Element]
  -> [UI Element]
  -> [UI Element]
  -> UI Element
topNavigation nodesSelector viewModeSelector metricsSelector =
  UI.div #. "w3-bar w3-large top-bar" #+
    [ UI.anchor #. "w3-bar-item" # set UI.href "https://iohk.io/" #+
        [ UI.img #. "iohk-logo" # set UI.src "/static/images/iohk-logo.png" #+ []
        ]
    , UI.div #. "w3-dropdown-hover" #+
        [ UI.button #. "w3-button view-mode-button"
                    # set UI.id_ "viewModeButton"
                    # set UI.value "paneMode"
                    #+ [string "View mode ▾"]
        , UI.div #. "w3-dropdown-content w3-bar-block w3-card-4" #+ viewModeSelector
        ]
    , UI.div #. "w3-dropdown-hover" #+
        [ UI.button #. "w3-button select-node-button" #+ [string "Select node ▾"]
        , UI.div #. "w3-dropdown-content w3-bar-block w3-card-4" #+ nodesSelector
        ]
    , UI.div #. "w3-dropdown-hover" # set UI.id_ "selectMetricButton" # hideIt #+
        [ UI.button #. "w3-button select-metric-button" #+ [string "Select metric ▾"]
        , UI.div #. "w3-dropdown-content w3-bar-block w3-card-4" #+ metricsSelector
        ]
    , UI.span #. "w3-right service-name" #+
        [ string "Cardano Node Real-time View"
        ]
    ]

forNode
  :: Text
  -> [(Text, Element, NodeStateElements, [PeerInfoItem])]
  -> (UI Element -> UI Element)
  -> UI ()
forNode nameOfNode nodeWidgetsWithElems action =
  forM_ nodeWidgetsWithElems $ \(aName, widget, _, _) ->
    when (aName == nameOfNode) $
      void $ element widget # action

forNodeColumn
  :: UI.Window
  -> Text
  -> (UI Element -> UI Element)
  -> UI ()
forNodeColumn window nameOfNode action = do
  let cellsIdsForNodeColumn =
        map (\elemName -> show elemName <> "-" <> T.unpack nameOfNode)
            allMetricsNames
  let allCells = ("gridNodeTH-" <> T.unpack nameOfNode) : cellsIdsForNodeColumn
  forM_ allCells $ \anId ->
    forElementWithId window anId action

showAllNodes
  :: UI.Window
  -> [(Text, Element, NodeStateElements, [PeerInfoItem])]
  -> UI ()
showAllNodes window nodeWidgetsWithElems = do
  forM_ nodeWidgetsWithElems $ \(_, widget, _, _) ->
    void $ element widget # showIt
  nodesCheckboxes <- UI.getElementsByClassName window "select-node-check"
  forM_ nodesCheckboxes $ \checkbox ->
    void $ element checkbox # set UI.checked True

showAllNodesColumns
  :: UI.Window
  -> [(Text, Element, NodeStateElements, [PeerInfoItem])]
  -> UI ()
showAllNodesColumns window nodeWidgetsWithElems = do
  forM_ nodeWidgetsWithElems $ \(nameOfNode, _, _, _) ->
    forNodeColumn window nameOfNode showCell
  nodesCheckboxes <- UI.getElementsByClassName window "select-node-check"
  forM_ nodesCheckboxes $ \checkbox ->
    void $ element checkbox # set UI.checked True

toggleViewMode
  :: UI.Window
  -> String
  -> Element
  -> [UI Element]
  -> [UI Element]
  -> UI ()
toggleViewMode window newValue rootElem childrenToAdd childrenToDelete = do
  -- Store current view mode in the view mode button.
  forElementWithId window "viewModeButton" (set UI.value newValue)
  -- Delete these elements from DOM.
  mapM_ (fmap UI.delete) childrenToDelete
  -- Explicitly remove current children of rootElem and set the new ones.
  void $ element rootElem # set UI.children []
  void $ element rootElem #+ childrenToAdd

-- | If all checkboxes are checked - "Show all" button should be disabled.
--   If at least one of them are unchecked - "Show all" button should be enabled.
changeStatusOfShowAllButton
  :: UI.Window
  -> String
  -> String
  -> UI ()
changeStatusOfShowAllButton window anId aClass =
  UI.getElementById window anId >>= \case
    Just showAllButton -> do
      checkboxes <- UI.getElementsByClassName window aClass
      statuses <- mapM (UI.get UI.checked) checkboxes
      if all ((==) True) statuses
        then void $ element showAllButton # set UI.class_ "w3-bar-item w3-button w3-border-top w3-disabled"
        else void $ element showAllButton # set UI.class_ "w3-bar-item w3-button w3-border-top"
    Nothing -> return ()

mkMetricsSelector
  :: UI.Window
  -> UI [UI Element]
mkMetricsSelector window = do
  allMetricsButton <-
    UI.anchor #. "w3-bar-item w3-button w3-border-top w3-disabled"
              # set UI.id_ "showAllMetricsButton"
              # set UI.href "#"
              #+ [UI.string "Show all"]
  void $ UI.onEvent (UI.click allMetricsButton) $ \_ -> do
    showAllMetrics window allMetricsNames
    -- All metrics checkboxes are already shown, disable button again.
    void $ element allMetricsButton # set UI.class_ "w3-bar-item w3-button w3-border-top w3-disabled"

  checkboxes <-
    forM allMetricsNames $ \aName ->
      element <$> mkCheckbox window aName
  return $ checkboxes ++ [element allMetricsButton]

mkCheckbox
  :: UI.Window
  -> ElementName
  -> UI Element
mkCheckbox window elemName = do
  metricCheckbox
    <- UI.input #. "w3-check select-metric-check"
                # set UI.type_ "checkbox"
                # set UI.checked True
                #+ []
  void $ UI.onEvent (UI.checkedChange metricCheckbox) $ \isChecked -> do
    let action = if isChecked then showRow else hideIt
    forElementWithId window (show elemName) action
    changeStatusOfShowAllButton window "showAllMetricsButton" "select-metric-check"

  metricArea
    <- UI.div #. "select-metric-check-area" #+
         [ element metricCheckbox
         , UI.label #+ [UI.string $ metricLabel elemName]
         ]
  return metricArea

forElementWithId
  :: UI.Window
  -> String
  -> (UI Element -> UI Element)
  -> UI ()
forElementWithId window anId action =
  UI.getElementById window anId >>= \case
    Just el -> void $ element el # action
    Nothing -> return ()

showAllMetrics
  :: UI.Window
  -> [ElementName]
  -> UI ()
showAllMetrics window metricsElems = do
  forM_ metricsElems $ \elemName ->
    forElementWithId window (show elemName) showRow
  metricsCheckboxes <- UI.getElementsByClassName window "select-metric-check"
  forM_ metricsCheckboxes $ \checkbox ->
    void $ element checkbox # set UI.checked True

showIt, hideIt, showRow, showCell :: UI Element -> UI Element
showIt   = set UI.style [("display", "block")]
hideIt   = set UI.style [("display", "none")]
showRow  = set UI.style [("display", "table-row")]
showCell = set UI.style [("display", "table-cell")]
