{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans -Wno-unticked-promoted-constructors #-}

module Cardano.Benchmarking.TxGenerator.NodeToNode
  ( BenchmarkTxSubmitTracers (..)
  , SendRecvConnect
  , SendRecvTxSubmission
  , benchmarkConnectTxSubmit
  ) where

import           Cardano.Prelude (Void, forever)
import           Prelude

import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (DeserialiseFailure)
import           Control.Monad.Class.MonadTimer (MonadTimer, threadDelay)
import           Data.ByteString.Lazy (ByteString)
import           Data.Map as Map
import           Data.Proxy (Proxy (..))
import           Network.Mux (MuxMode (InitiatorMode), WithMuxBearer (..))
import           Network.Socket (AddrInfo (..), SockAddr)

import           Control.Tracer (Tracer (..), nullTracer)
import           Ouroboros.Consensus.Byron.Ledger.Mempool (GenTx)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (supportedNodeToNodeVersions)

import           Ouroboros.Consensus.Network.NodeToNode (Codecs (..), defaultCodecs)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo, pClientInfoCodecConfig)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Network.Channel (Channel (..))
import           Ouroboros.Network.Driver (TraceSendRecv (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.Mux (MuxPeer (..), OuroborosApplication (..),
                                        RunMiniProtocol (..))
import           Ouroboros.Network.NodeToClient (IOManager, chainSyncPeerNull)
import           Ouroboros.Network.NodeToNode (NetworkConnectTracers (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Client (BlockFetchClient (..),
                                                               blockFetchClientPeer)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (Versions, simpleSingletonVersions)
import           Ouroboros.Network.Protocol.TxSubmission.Client (TxSubmissionClient,
                                                                 txSubmissionClientPeer)
import           Ouroboros.Network.Snocket (socketSnocket)

import           Cardano.Api.Typed (NetworkId)
import qualified Cardano.Api.Typed as Api
import           Cardano.Benchmarking.TxGenerator.Types

type SendRecvConnect = WithMuxBearer
                         NtN.RemoteConnectionId
                         (TraceSendRecv (Handshake
                                           NtN.NodeToNodeVersion
                                           CBOR.Term))

data BenchmarkTxSubmitTracers m blk = BenchmarkTracers
    { trSendRecvConnect      :: Tracer m SendRecvConnect
    , trSendRecvTxSubmission :: Tracer m (SendRecvTxSubmission blk)
    }

benchmarkConnectTxSubmit
  :: forall m blk . (RunNode blk, m ~ IO)
  => IOManager
  -> BenchmarkTxSubmitTracers m blk
  -- ^ For tracing the send/receive actions
  -> ProtocolClientInfo blk
  -- ^ The particular block protocol
  -> NetworkId
  -- ^ Network(Magic)
  -> Maybe AddrInfo
  -- ^ local address information (typically local interface/port to use)
  -> AddrInfo
  -- ^ remote address information
  -> TxSubmissionClient (GenTxId blk) (GenTx blk) m ()
  -- ^ the particular txSubmission peer
  -> m ()
benchmarkConnectTxSubmit iocp trs cfg network localAddr remoteAddr myTxSubClient =
  NtN.connectTo
    (socketSnocket iocp)
    NetworkConnectTracers {
        nctMuxTracer       = nullTracer,
        nctHandshakeTracer = trSendRecvConnect trs
      }
    peerMultiplex
    (addrAddress <$> localAddr)
    (addrAddress remoteAddr)
 where
  myCodecs :: Codecs blk DeserialiseFailure m
                ByteString ByteString ByteString ByteString ByteString ByteString
  myCodecs  = defaultCodecs
                  (pClientInfoCodecConfig cfg)
                  ((Map.!) (supportedNodeToNodeVersions $ Proxy @blk ) maxBound)
  peerMultiplex :: Versions NtN.NodeToNodeVersion NtN.DictVersion
                     (OuroborosApplication InitiatorMode SockAddr ByteString IO () Void)
  peerMultiplex =
    simpleSingletonVersions
      NtN.NodeToNodeV_1
      (NtN.NodeToNodeVersionData { NtN.networkMagic = toNetworkMagic network})
      (NtN.DictVersion NtN.nodeToNodeCodecCBORTerm) $
      NtN.nodeToNodeProtocols NtN.defaultMiniProtocolParameters ( \peer _->
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
          , NtN.txSubmissionProtocol = InitiatorProtocolOnly $
                                         MuxPeer
                                           (trSendRecvTxSubmission trs)
                                           (cTxSubmissionCodec myCodecs)
                                           (txSubmissionClientPeer myTxSubClient)
          , NtN.keepAliveProtocol = InitiatorProtocolOnly $
                                      MuxPeerRaw
                                        (aKeepAliveClient NtN.NodeToNodeV_1 peer)
          } )
        NtN.NodeToNodeV_1
    where
      -- Stolen from: Ouroboros/Consensus/Network/NodeToNode.hs
      aKeepAliveClient
        :: NtN.NodeToNodeVersion
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

toNetworkMagic :: NetworkId -> NetworkMagic
toNetworkMagic  Api.Mainnet     = NetworkMagic 764824073
toNetworkMagic (Api.Testnet nm) = nm
