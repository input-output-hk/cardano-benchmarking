{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.NodeState.Parsers
    ( extractPeersInfo
    , updateCurrentPeersInfo
    ) where

import           Cardano.Prelude

import qualified Data.Aeson as A
import           Data.Aeson
                   ( Object
                   , (.:)
                   )
import           Data.List
                   ( last )
import qualified Data.Text as T

import           Cardano.Benchmarking.RTView.NodeState.Types
                   ( PeerInfo (..) )

extractPeersInfo :: Object -> [PeerInfo]
extractPeersInfo peersObj =
  case result of
    A.Success connectedPeers ->
      let addrs = map (extractRemoteAddr . peer) $ peers connectedPeers
      in [PeerInfo (T.unpack addr) "" "" | addr <- addrs]
    A.Error _ -> []
 where
  result :: A.Result ConnectedPeers
  result = A.fromJSON $ A.Object peersObj

  -- We know that 'peer' contains 'Show'-result of 'ConnectionId'.
  extractRemoteAddr :: Text -> Text
  extractRemoteAddr = T.init . last . T.words

-- | Check if such peer already connected, and if so - take its slotNum and blockNum.
updateCurrentPeersInfo :: [PeerInfo] -> [PeerInfo] -> [PeerInfo]
updateCurrentPeersInfo currentPeersInfo [] = currentPeersInfo
updateCurrentPeersInfo currentPeersInfo newPeersInfo =
  flip map newPeersInfo $ \newPI@(PeerInfo connId _ _) ->
    let existingPI = find (\(PeerInfo cId _ _) -> cId == connId) currentPeersInfo
    in case existingPI of
         Nothing -> newPI
         Just pI -> PeerInfo connId (piSlotNumber pI) (piBlockNumber pI)

-- Types for decoding from JSON-representation. The only information we need here is ConnectionId.

data ConnectedPeers = ConnectedPeers
  { peers :: ![ConnectedPeer]
  }

data ConnectedPeer = ConnectedPeer
  { peer :: !Text
  }

instance A.FromJSON ConnectedPeers where
  parseJSON = A.withObject "ConnectedPeers" $ \v -> ConnectedPeers
    <$> v .: "peers"

instance A.FromJSON ConnectedPeer where
  parseJSON = A.withObject "ConnectedPeer" $ \v -> ConnectedPeer
    <$> v .: "peer"
