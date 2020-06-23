{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.GUI.Markup
    ( mkPageBody
    ) where

import           Cardano.Prelude

import qualified Data.Text as T

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
                   ( Element, UI
                   , (#), (#+), (#.)
                   , element, set, string
                   )

import           Cardano.BM.Data.Configuration
                   ( RemoteAddrNamed (..) )
import           Cardano.Benchmarking.RTView.GUI.Elements
                   ( NodeStateElements, NodesStateElements
                   , PeerInfoItem
                   )
import           Cardano.Benchmarking.RTView.GUI.NodeWidget
                   ( mkNodeWidget )

mkPageBody
  :: UI.Window
  -> [RemoteAddrNamed]
  -> UI (Element, NodesStateElements)
mkPageBody window acceptors = do
  -- Create widgets for each node (corresponding to acceptors).
  nodeWidgetsWithElems
    <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
         (widget, nodeStateElems, peerInfoItems) <- mkNodeWidget
         return (nameOfNode, widget, nodeStateElems, peerInfoItems)

  -- Create widgets areas on the page.
  widgetsAreas
    <- forM nodeWidgetsWithElems $ \(_, widget, _, _) ->
         return $ UI.div #. "w3-col l6 m12 s12" #+ [element widget]

  -- Register clickable selector for nodes (to be able to show only one or all of them).
  nodesSelector <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
    nodeButton <- UI.anchor #. "w3-bar-item w3-button" # set UI.href "#" #+ [UI.string $ T.unpack nameOfNode]
    void $ UI.onEvent (UI.click nodeButton) $ \_ ->
      -- Make only widget for nameOfNode visible.
      showOnlyOneWidget nameOfNode nodeWidgetsWithElems
    return $ element nodeButton

  -- Add an additional button to show all widgets again (if user already clicked to some node previously).
  -- It makes sense only if there's more than one node.
  additionalButton
    <- if length nodesSelector > 1
         then do
           allNodesButton <- UI.anchor #. "w3-bar-item w3-button w3-border-top" # set UI.href "#" #+ [UI.string "Show all"]
           void $ UI.onEvent (UI.click allNodesButton) $ \_ ->
             -- Make all widgets visible again.
             showAllWidgetsAgain nodeWidgetsWithElems
           return [element allNodesButton]
         else
           return []

  let allSelectors = nodesSelector ++ additionalButton

  -- Make page body.
  body
    <- UI.getBody window #+
         [ topNavigation allSelectors
         , UI.div #. "w3-row" #+ widgetsAreas
         ]

  nodesStateElems
    <- forM nodeWidgetsWithElems $ \(nameOfNode, _, nodeStateElems, peerInfoItems) ->
         return (nameOfNode, nodeStateElems, peerInfoItems)

  return (body, nodesStateElems)

topNavigation
  :: [UI Element]
  -> UI Element
topNavigation nodesSelector = do
  UI.div #. "w3-bar w3-large top-bar" #+
    [ UI.anchor #. "w3-bar-item" # set UI.href "https://iohk.io/" #+
        [ UI.img #. "iohk-logo" # set UI.src "/static/images/iohk-logo.png" #+ []
        ]
    , UI.div #. "w3-dropdown-hover" #+
        [ UI.button #. "w3-button select-node-button" #+ [string "Select node ▾"]
        , UI.div #. "w3-dropdown-content w3-bar-block w3-card-4" #+ nodesSelector
        ]
    , UI.span #. "w3-right service-name" #+
        [ string "Cardano Node Real-time View"
        ]
    ]

-- | If the number of connected nodes is big, it can be convenient
--   to choose only one of them.
showOnlyOneWidget
  :: Text
  -> [(Text, Element, NodeStateElements, [PeerInfoItem])]
  -> UI ()
showOnlyOneWidget nameOfNode nodeWidgetsWithElems =
  forM_ nodeWidgetsWithElems $ \(aName, widget, _, _) ->
    if aName == nameOfNode
      then void $ element widget # showIt
      else void $ element widget # hideIt

-- | If user choosed one particular node, it's possible to
--   show all nodes again without page reload.
showAllWidgetsAgain
  :: [(Text, Element, NodeStateElements, [PeerInfoItem])]
  -> UI ()
showAllWidgetsAgain nodeWidgetsWithElems =
  forM_ nodeWidgetsWithElems $ \(_, widget, _, _) -> do
    void $ element widget # showIt

showIt, hideIt :: UI Element -> UI Element
showIt = set UI.style [("display", "block")]
hideIt = set UI.style [("display", "none")]
