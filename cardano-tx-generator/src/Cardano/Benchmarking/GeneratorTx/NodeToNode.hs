{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans -Wno-unticked-promoted-constructors -Wno-all-missed-specialisations #-}

module Cardano.Benchmarking.GeneratorTx.NodeToNode
  ( benchmarkConnectTxSubmit
  ) where

import           Cardano.Prelude (Void, forever)
import           Prelude

import           Codec.Serialise (DeserialiseFailure)
import           Control.Monad.Class.MonadTimer (MonadTimer, threadDelay)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import           Data.Proxy (Proxy (..))
import           Network.Mux (MuxMode (InitiatorMode))
import           Network.Socket (AddrInfo (..), SockAddr)

import           Control.Tracer (nullTracer)
import           Ouroboros.Consensus.Byron.Ledger.Mempool (GenTx)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Network.NodeToNode (Codecs (..), defaultCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (RunNode)

import           Ouroboros.Network.Channel (Channel (..))
import           Ouroboros.Network.Mux (MuxPeer (..), OuroborosApplication (..),
                                        RunMiniProtocol (..))
import           Ouroboros.Network.NodeToClient (chainSyncPeerNull)
import           Ouroboros.Network.NodeToNode (NetworkConnectTracers (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Client (BlockFetchClient (..),
                                                               blockFetchClientPeer)
import           Ouroboros.Network.Protocol.Handshake.Version (Versions, simpleSingletonVersions)
import           Ouroboros.Network.Protocol.TxSubmission.Client (TxSubmissionClient,
                                                                 txSubmissionClientPeer)
import           Ouroboros.Network.Snocket (socketSnocket)

import           Cardano.Benchmarking.GeneratorTx.Era


benchmarkConnectTxSubmit
  :: forall m mode era blk. (blk ~ HFCBlockOf mode, RunNode blk, m ~ IO)
  => Mode mode era
  -> Maybe AddrInfo
  -- ^ local address information (typically local interface/port to use)
  -> AddrInfo
  -- ^ remote address information
  -> TxSubmissionClient (GenTxId blk) (GenTx blk) m ()
  -- ^ the particular txSubmission peer
  -> m ()
benchmarkConnectTxSubmit p localAddr remoteAddr myTxSubClient =
  NtN.connectTo
    (socketSnocket $ modeIOManager p)
    NetworkConnectTracers {
        nctMuxTracer       = nullTracer,
        nctHandshakeTracer = trConnect p
      }
    peerMultiplex
    (addrAddress <$> localAddr)
    (addrAddress remoteAddr)
 where
  modeVer :: Mode mode era -> NodeToNodeVersion
  modeVer = \case
    ModeCardanoByron{}   -> NodeToNodeV_2
    ModeCardanoShelley{} -> NodeToNodeV_2
    ModeByron{}          -> NodeToNodeV_1
    ModeShelley{}        -> NodeToNodeV_1
  n2nVer :: NodeToNodeVersion
  n2nVer = modeVer p
  blkN2nVer :: BlockNodeToNodeVersion blk
  blkN2nVer = supportedVers Map.! n2nVer
  supportedVers :: Map.Map NodeToNodeVersion (BlockNodeToNodeVersion blk)
  supportedVers = supportedNodeToNodeVersions (Proxy @blk)
  myCodecs :: Codecs blk DeserialiseFailure m
                ByteString ByteString ByteString ByteString ByteString ByteString
  myCodecs  = defaultCodecs (modeCodecConfig p) blkN2nVer
  peerMultiplex :: Versions NtN.NodeToNodeVersion NtN.DictVersion
                     (OuroborosApplication InitiatorMode SockAddr ByteString IO () Void)
  peerMultiplex =
    simpleSingletonVersions
      n2nVer
      (NtN.NodeToNodeVersionData { NtN.networkMagic = modeNetworkMagicN2N p})
      (NtN.DictVersion NtN.nodeToNodeCodecCBORTerm) $
      NtN.nodeToNodeProtocols NtN.defaultMiniProtocolParameters ( \them _ ->
        NtN.NodeToNodeProtocols
          { NtN.chainSyncProtocol = InitiatorProtocolOnly $
                                      MuxPeer
                                        nullTracer
                                        (cChainSyncCodec myCodecs)
                                        chainSyncPeerNull
          , NtN.blockFetchProtocol = InitiatorProtocolOnly $
                                       MuxPeer
                                         nullTracer
                                         (cBlockFetchCodec myCodecs)
                                         (blockFetchClientPeer blockFetchClientNull)
          , NtN.keepAliveProtocol = InitiatorProtocolOnly $
                                      MuxPeerRaw
                                        (aKeepAliveClient n2nVer them)
          , NtN.txSubmissionProtocol = InitiatorProtocolOnly $
                                         MuxPeer
                                           (trSubmitMux p)
                                           (cTxSubmissionCodec myCodecs)
                                           (txSubmissionClientPeer myTxSubClient)
          } )
        n2nVer
  -- Stolen from: Ouroboros/Consensus/Network/NodeToNode.hs
  aKeepAliveClient
    :: NodeToNodeVersion
    -> remotePeer
    -> Channel m bKA
    -> m ((), Maybe bKA)
  aKeepAliveClient _version _them _channel =
    forever (threadDelay 1000) >> return ((), Nothing)

-- the null block fetch client
blockFetchClientNull
  :: forall block m a.  MonadTimer m
  => BlockFetchClient block m a
blockFetchClientNull
  = BlockFetchClient $ forever $ threadDelay (24 * 60 * 60) {- one day in seconds -}
