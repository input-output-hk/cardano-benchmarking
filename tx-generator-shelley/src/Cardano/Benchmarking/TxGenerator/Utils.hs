{-# LANGUAGE OverloadedStrings #-}
module Cardano.Benchmarking.TxGenerator.Utils
where

import qualified Data.IP as IP
import           Data.Text (Text)
import           Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Family (..), SocketType (Stream),
                                 addrFamily, addrFlags, addrSocketType, defaultHints, getAddrInfo)

import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (appendName)
import           Cardano.Config.Types (NodeAddress (..), NodeHostAddress (..))
import           Cardano.Node.Logging (LoggingLayer (..), Trace)

import           Control.Tracer (Tracer)

import           Ouroboros.Consensus.Ledger.SupportsMempool as Mempool (GenTxId)

import           Cardano.Benchmarking.TxGenerator.NodeToNode (SendRecvConnect)
import           Cardano.Benchmarking.TxGenerator.Types

createTracers
  :: LoggingLayer
  -> ( Tracer IO (TraceBenchTxSubmit (Mempool.GenTxId Block))
     , Tracer IO SendRecvConnect
     , Tracer IO (SendRecvTxSubmission Block)
     , Tracer IO TraceLowLevelSubmit
     , Tracer IO NodeToNodeSubmissionTrace
     )
createTracers loggingLayer =
  (benchTracer, connectTracer, submitTracer, lowLevelSubmitTracer, n2nSubmitTracer)
 where
  basicTr :: Trace IO Text
  basicTr = llBasicTrace loggingLayer

  tr :: Trace IO Text
  tr = llAppendName loggingLayer "cli" basicTr

  tr' :: Trace IO Text
  tr' = appendName "generate-txs" tr

  benchTracer :: Tracer IO (TraceBenchTxSubmit (Mempool.GenTxId Block))
  benchTracer = toLogObjectVerbose (appendName "benchmark" tr')

  connectTracer :: Tracer IO SendRecvConnect
  connectTracer = toLogObjectVerbose (appendName "connect" tr')

  submitTracer :: Tracer IO (SendRecvTxSubmission Block)
  submitTracer = toLogObjectVerbose (appendName "submit" tr')

  lowLevelSubmitTracer :: Tracer IO TraceLowLevelSubmit
  lowLevelSubmitTracer = toLogObjectVerbose (appendName "llSubmit" tr')

  n2nSubmitTracer :: Tracer IO NodeToNodeSubmissionTrace
  n2nSubmitTracer = toLogObjectVerbose (appendName "submit2" tr')

-- | It represents the earliest time at which another tx will be sent.
minimalTPSRate :: TPSRate -> DiffTime
minimalTPSRate (TPSRate tps) = picosecondsToDiffTime timeInPicoSecs
 where
  -- Since tps cannot be less than 0.05, timeInPicoSecs
  -- will definitely be integer number after round.
  timeInPicoSecs = round $ picosecondsIn1Sec / tps
  picosecondsIn1Sec = 1000000000000 :: Float

resolveRemoteAddr :: NodeAddress -> IO AddrInfo
resolveRemoteAddr name = do
  addrs <-getAddrInfo (Just hints) (Just targetNodeHost) (Just targetNodePort)
  case addrs of
    [] -> Prelude.error $ "getRemoteAddr : unknown: " ++ show name
    h:_ -> return h
  where
    (anAddrFamily, targetNodeHost) = case unNodeHostAddress $ naHostAddress name of
              Just (IP.IPv4 ipv4) -> (AF_INET,  show ipv4)
              Just (IP.IPv6 ipv6) -> (AF_INET6, show ipv6)
              _ -> error "Target node's IP-address is undefined!"

    targetNodePort = show $ naPort name

    hints :: AddrInfo
    hints = defaultHints
          { addrFlags      = [AI_PASSIVE]
          , addrFamily     = anAddrFamily
          , addrSocketType = Stream
          , addrCanonName  = Nothing
          }
