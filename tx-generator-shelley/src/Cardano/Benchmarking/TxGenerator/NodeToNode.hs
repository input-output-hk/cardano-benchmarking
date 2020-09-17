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

import           Cardano.Prelude (Void, atomically, forever, liftIO)
import           Prelude

import           Codec.Serialise (DeserialiseFailure)
import           Control.Monad.Class.MonadTimer (MonadTimer, threadDelay)
import           Control.Monad.Class.MonadSTM.Strict (newTVar)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import           Data.Proxy (Proxy (..))
import           Network.Mux (MuxMode (InitiatorMode), WithMuxBearer)
import           Network.Socket (AddrInfo (..), SockAddr)
import           System.Random (newStdGen)

import           Control.Tracer (Tracer, nullTracer)
import           Ouroboros.Consensus.Byron.Ledger.Mempool (GenTx)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Network.NodeToNode -- (Codecs (..), defaultCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (RunNode)

import           Ouroboros.Network.Channel (Channel (..))
import           Ouroboros.Network.DeltaQ (defaultGSV)
import           Ouroboros.Network.Driver (runPeerWithLimits)
import           Ouroboros.Network.KeepAlive
import           Ouroboros.Network.Mux (MuxPeer (..), OuroborosApplication (..),
                                        RunMiniProtocol (..), continueForever)
import           Ouroboros.Network.NodeToClient (chainSyncPeerNull)
import           Ouroboros.Network.NodeToNode (NetworkConnectTracers (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Client (BlockFetchClient (..),
                                                               blockFetchClientPeer)
import           Ouroboros.Network.Protocol.Handshake.Version (Versions, simpleSingletonVersions)
import           Ouroboros.Network.Protocol.KeepAlive.Codec
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.TxSubmission.Client (TxSubmissionClient,
                                                                 txSubmissionClientPeer)
import           Ouroboros.Network.Snocket (socketSnocket)

import           Cardano.Api.Typed (NetworkId)
import qualified Cardano.Api.Typed as Api

import qualified Codec.CBOR.Term as CBOR
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Network.NodeToClient (Handshake, IOManager)
import           Ouroboros.Network.Driver (TraceSendRecv (..))

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
                                        (kaClient NtN.NodeToNodeV_1 peer)
          } )
        NtN.NodeToNodeV_1
    where
  -- Stolen from: Ouroboros/Consensus/Network/NodeToNode.hs
  kaClient
    :: Ord remotePeer
    => NodeToNodeVersion
    -> remotePeer
    -> Channel m ByteString
    -> m ((), Maybe ByteString)
  kaClient version them channel = do
    case version of
      -- Version 1 doesn't support keep alive protocol but Blockfetch
      -- still requires a PeerGSV per peer.
      NodeToNodeV_1 -> forever (threadDelay 1000) >> return ((), Nothing)
      NodeToNodeV_2 -> forever (threadDelay 1000) >> return ((), Nothing)
      _             -> do
        keepAliveRng <- newStdGen
        peerGSVMap <- liftIO . atomically . newTVar $ Map.singleton them defaultGSV
        runPeerWithLimits
          nullTracer
          (cKeepAliveCodec myCodecs)
          (byteLimitsKeepAlive (const 0)) -- TODO: Real Bytelimits, see #1727
          timeLimitsKeepAlive
          channel
          $ keepAliveClientPeer
          $ keepAliveClient
              nullTracer
              keepAliveRng
              (continueForever (Proxy :: Proxy m)) them peerGSVMap
              (KeepAliveInterval 10)

-- the null block fetch client
blockFetchClientNull
  :: forall block m a.  MonadTimer m
  => BlockFetchClient block m a
blockFetchClientNull
  = BlockFetchClient $ forever $ threadDelay (24 * 60 * 60) {- one day in seconds -}

toNetworkMagic :: NetworkId -> Api.NetworkMagic
toNetworkMagic  Api.Mainnet     = Api.NetworkMagic 764824073
toNetworkMagic (Api.Testnet nm) = nm
