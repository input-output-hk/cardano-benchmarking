{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.GeneratorTx.Submission
  ( SubmissionParams(..)
  , Submission(sParams)
  , SubmissionThreadReport
  , mkSubmission
  , mkSubmissionSummary
  , submitThreadReport
  , txSubmissionClient
  , simpleTxFeeder
  , tpsLimitedTxFeeder
  ) where

import           Cardano.Prelude hiding (ByteString, atomically, retry, threadDelay)
import           Prelude (String, fail)

import           Control.Arrow ((&&&))
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TBQueue (TBQueue)
import           Control.Monad (replicateM)

import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as Clock

import           Control.Tracer (Tracer, traceWith)

import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Common ()
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Cardano.Tracing.OrphanInstances.Network ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import           Ouroboros.Consensus.Ledger.SupportsMempool (txInBlockSize)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Mempool

import           Ouroboros.Network.Protocol.TxSubmission.Client (ClientStIdle (..),
                                                                 ClientStTxIds (..),
                                                                 ClientStTxs (..),
                                                                 TxSubmissionClient (..))
import           Ouroboros.Network.Protocol.TxSubmission.Type (BlockingReplyList (..),
                                                               TokBlockingStyle (..), TxSizeInBytes)

import           Cardano.Api.Typed

import           Cardano.Benchmarking.GeneratorTx.Era
import           Cardano.Benchmarking.GeneratorTx.Benchmark
import           Cardano.Benchmarking.GeneratorTx.Tx


{-------------------------------------------------------------------------------
  Parametrisation & state
-------------------------------------------------------------------------------}

data SubmissionParams
  = SubmissionParams
      { spTps           :: !TPSRate
      , spTargets       :: !Natural
      , spQueueLen      :: !Natural
      , spErrorPolicy   :: !SubmissionErrorPolicy
      }

data Submission (m :: Type -> Type) (era :: Type)
  = Submission
      { sParams      :: !SubmissionParams
      , sStartTime   :: !UTCTime
      , sThreads     :: !Natural
      , sTxSendQueue :: !(TBQueue (Maybe (Tx era)))
      , sReportsRefs :: ![STM.TMVar (Either String SubmissionThreadReport)]
      , sTrace       :: !(Tracer m (TraceBenchTxSubmit TxId))
      }

mkSubmission
  :: MonadIO m
  => Tracer m (TraceBenchTxSubmit TxId)
  -> SubmissionParams
  -> m (Submission m era)
mkSubmission sTrace sParams@SubmissionParams{spTargets=sThreads, spQueueLen} = liftIO $ do
  sStartTime <- Clock.getCurrentTime
  sTxSendQueue <- STM.newTBQueueIO spQueueLen
  sReportsRefs <- STM.atomically $ replicateM (fromIntegral sThreads) STM.newEmptyTMVar
  pure Submission{..}

submitThreadReport
  :: MonadIO m
  => Submission m era
  -> Natural
  -> Either String SubmissionThreadReport
  -> m ()
submitThreadReport Submission{sReportsRefs} threadIx =
 liftIO . STM.atomically . STM.putTMVar (sReportsRefs L.!! fromIntegral threadIx)

{-------------------------------------------------------------------------------
  Results
-------------------------------------------------------------------------------}
data SubmissionThreadStats
  = SubmissionThreadStats
      { stsAcked       :: {-# UNPACK #-} !Ack
      , stsSent        :: {-# UNPACK #-} !Sent
      , stsUnavailable :: {-# UNPACK #-} !Unav
      }

data SubmissionThreadReport
  = SubmissionThreadReport
      { strStats         :: !SubmissionThreadStats
      , strThreadIndex   :: !Natural
      , strEndOfProtocol :: !UTCTime
      }

mkSubmissionSummary
  :: MonadIO m
  => Submission m tx
  -> m SubmissionSummary
mkSubmissionSummary
  Submission{ sStartTime, sReportsRefs}
 = do
  results <- sequence (liftIO . STM.atomically . STM.readTMVar <$> sReportsRefs)
  let (failures, reports) = partitionEithers results
  now <- liftIO Clock.getCurrentTime
  let ssElapsed = Clock.diffUTCTime now sStartTime
      ssTxSent@(Sent sent) = sum $ stsSent . strStats <$> reports
      ssTxUnavailable = sum $ stsUnavailable . strStats <$> reports
      ssEffectiveTps = txDiffTimeTPS sent ssElapsed
      ssThreadwiseTps = threadReportTps <$> reports
      ssFailures = failures
  pure SubmissionSummary{..}
 where
   txDiffTimeTPS :: Int -> NominalDiffTime -> TPSRate
   txDiffTimeTPS n delta =
     TPSRate $ realToFrac $ fromIntegral n / delta

   threadReportTps :: SubmissionThreadReport -> TPSRate
   threadReportTps
     SubmissionThreadReport
       { strStats=SubmissionThreadStats{stsAcked=Ack ack}, strEndOfProtocol } =
         txDiffTimeTPS ack (Clock.diffUTCTime strEndOfProtocol sStartTime)

{-------------------------------------------------------------------------------
  Submission queue:  feeding and consumption
-------------------------------------------------------------------------------}
simpleTxFeeder
  :: forall m era
  . (MonadIO m)
  => Submission m era -> [Tx era] -> m ()
simpleTxFeeder
 Submission{sTrace, sThreads, sTxSendQueue} txs = do
  foldM_ (const feedTx) () (zip txs [0..])
  -- Issue the termination notifications.
  replicateM_ (fromIntegral sThreads) $
    liftIO $ STM.atomically $ STM.writeTBQueue sTxSendQueue Nothing
 where
   feedTx :: (Tx era, Int) -> m ()
   feedTx (tx, ix) = do
     liftIO $ STM.atomically $ STM.writeTBQueue sTxSendQueue (Just tx)
     traceWith sTrace $ TraceBenchTxSubServFed [getTxId $ getTxBody tx] ix

tpsLimitedTxFeeder
  :: forall m era
  . (MonadIO m)
  => Submission m era -> [Tx era] -> m ()
tpsLimitedTxFeeder
 Submission{ sParams=SubmissionParams{spTps=TPSRate rate}
           , sThreads
           , sTrace
           , sTxSendQueue } txs = do
  now <- liftIO Clock.getCurrentTime
  foldM_ feedTx (now, 0) (zip txs [0..])
  -- Issue the termination notifications.
  replicateM_ (fromIntegral sThreads) .
    liftIO . STM.atomically $ STM.writeTBQueue sTxSendQueue Nothing
 where
   feedTx :: (UTCTime, NominalDiffTime)
          -> (Tx era, Int)
          -> m (UTCTime, NominalDiffTime)
   feedTx (lastPreDelay, lastDelay) (tx, ix) = do
     liftIO . STM.atomically $ STM.writeTBQueue sTxSendQueue (Just tx)
     traceWith sTrace $ TraceBenchTxSubServFed [getTxId $ getTxBody tx] ix
     now <- liftIO Clock.getCurrentTime
     let targetDelay = realToFrac $ 1.0 / rate
         loopCost = (now `Clock.diffUTCTime` lastPreDelay) - lastDelay
         delay = targetDelay - loopCost
     liftIO . threadDelay . ceiling $ (realToFrac delay * 1000000.0 :: Double)
     pure (now, delay)

consumeTxs
  :: forall m blk era
  . (MonadIO m)
  => Submission m era -> TokBlockingStyle blk -> Req -> m (Bool, UnReqd (Tx era))
consumeTxs Submission{sTxSendQueue} blk req
  = liftIO . STM.atomically $ go blk req []
 where
   go :: TokBlockingStyle a -> Req -> [Tx era] -> STM (Bool, UnReqd (Tx era))
   go _ 0 acc = pure (False, UnReqd acc)
   go TokBlocking n acc = STM.readTBQueue sTxSendQueue >>=
     \case
       Nothing -> pure (True, UnReqd acc)
       Just tx -> go TokBlocking (n - 1) (tx:acc)
   go TokNonBlocking _ _ = STM.tryReadTBQueue sTxSendQueue >>=
     \case
       Nothing -> pure (False, UnReqd [])
       Just Nothing -> pure (True, UnReqd [])
       Just (Just tx) -> pure (False, UnReqd [tx])

{-------------------------------------------------------------------------------
  The submission client
-------------------------------------------------------------------------------}
txSubmissionClient
  :: forall m mode era tx txid gentx gentxid .
     ( MonadIO m, MonadFail m
     , ConfigSupportsTxGen mode era
     , tx      ~ Tx era
     , txid    ~ TxId
     , gentx   ~ GenTxOf mode
     , gentxid ~ GenTxIdOf mode
     )
  => Mode mode era
  -> Tracer m NodeToNodeSubmissionTrace
  -> Tracer m (TraceBenchTxSubmit txid)
  -> Submission m era
  -> Natural
  -- This return type is forced by Ouroboros.Network.NodeToNode.connectTo
  -> TxSubmissionClient gentxid gentx m ()
txSubmissionClient m tr bmtr sub threadIx =
  TxSubmissionClient $
    pure $ client False (UnAcked []) (SubmissionThreadStats 0 0 0)
 where
   -- Nothing means we've ran out of things to either announce or send.
   decideAnnouncement :: TokBlockingStyle a
                      -> Ack -> UnReqd tx -> UnAcked tx
                      -> m (Either Text (ToAnnce tx, UnAcked tx, Acked tx))
   decideAnnouncement b (Ack ack) (UnReqd annNow) (UnAcked unAcked) =
     if   tokIsBlocking b && ack /= length unAcked
     then pure $ Left "decideAnnouncement: TokBlocking, but length unAcked != ack"
     else pure $ Right (ToAnnce annNow, UnAcked newUnacked, Acked acked)
       where
         stillUnacked, newUnacked, acked :: [tx]
         (stillUnacked, acked) = L.splitAtEnd ack unAcked
         newUnacked = annNow <> stillUnacked

   -- Sadly, we can't just return what we want, we instead have to
   -- communicate via IORefs, because..
   client :: Bool -> UnAcked tx -> SubmissionThreadStats
          -- The () return type is forced by Ouroboros.Network.NodeToNode.connectTo
          -> ClientStIdle gentxid gentx m ()
   client done unAcked (!stats) = ClientStIdle
    { recvMsgRequestTxIds = \blocking (protoToAck -> ack) (protoToReq -> req)
       -> do
        traceWith tr $ reqIdsTrace ack req blocking

        (exhausted, unReqd) <-
          if done then pure (True, UnReqd [])
          else consumeTxs sub blocking req

        r' <- decideAnnouncement blocking ack unReqd unAcked
        (ann@(ToAnnce annNow), newUnacked@(UnAcked outs), Acked acked)
          <- case r' of
            Left e -> traceWith bmtr (TraceBenchTxSubError e)
                      >> fail (T.unpack e)
            Right x -> pure x

        traceWith tr $ idListTrace ann blocking
        traceWith bmtr $ TraceBenchTxSubServAnn  (getTxId . getTxBody <$> annNow)
        traceWith bmtr $ TraceBenchTxSubServAck  (getTxId . getTxBody <$> acked)
        traceWith bmtr $ TraceBenchTxSubServOuts (getTxId . getTxBody <$> outs)

        let newStats = stats { stsAcked =
                               stsAcked stats + ack }

        case (exhausted, NE.nonEmpty annNow, blocking) of
          (_, Nothing, TokBlocking) -> do
            traceWith tr EndOfProtocol
            SendMsgDone <$> (submitReport newStats
                             -- The () return type is forced by
                             --   Ouroboros.Network.NodeToNode.connectTo
                             >> pure ())

          (_, Just neAnnNow, TokBlocking) ->
            pure $ SendMsgReplyTxIds
                     (BlockingReply $ txToIdSize <$> neAnnNow)
                     (client exhausted newUnacked newStats)

          (False, Nothing, TokNonBlocking) -> do
            pure $ SendMsgReplyTxIds
                     (NonBlockingReply [])
                     (client exhausted newUnacked newStats)

          (_, _, TokNonBlocking) ->
            pure $ SendMsgReplyTxIds
                     (NonBlockingReply $ txToIdSize <$> annNow)
                     (client exhausted newUnacked newStats)

    , recvMsgRequestTxs = \(fmap (fromGenTxId m) -> reqTxids) -> do
        traceWith tr $ ReqTxs (length reqTxids)
        let UnAcked ua = unAcked
            uaIds = getTxId . getTxBody <$> ua
            (toSend, _retained) = L.partition ((`L.elem` reqTxids) . getTxId . getTxBody) ua
            missIds = reqTxids L.\\ uaIds

        traceWith tr $ TxList (length toSend)
        traceWith bmtr $ TraceBenchTxSubServReq reqTxids
        traceWith bmtr $ TraceBenchTxSubServOuts (getTxId . getTxBody <$> ua)
        unless (L.null missIds) $
          traceWith bmtr $ TraceBenchTxSubServUnav missIds
        pure $ SendMsgReplyTxs (toGenTx m <$> toSend)
          (client done unAcked $
            stats { stsSent =
                    stsSent stats + Sent (length toSend)
                  , stsUnavailable =
                    stsUnavailable stats + Unav (length missIds)})
    , recvMsgKThxBye = do
        traceWith tr KThxBye
        void $ submitReport stats
    }

   submitReport :: SubmissionThreadStats -> m SubmissionThreadReport
   submitReport strStats = do
     strEndOfProtocol <- liftIO Clock.getCurrentTime
     let strThreadIndex = threadIx
         report = SubmissionThreadReport{..}
     submitThreadReport sub threadIx (Right report)
     pure report

   txToIdSize :: tx -> (gentxid, TxSizeInBytes)
   txToIdSize = (Mempool.txId &&& txInBlockSize) . toGenTx m

   protoToAck :: Word16 -> Ack
   protoToAck = Ack . fromIntegral

   protoToReq :: Word16 -> Req
   protoToReq = Req . fromIntegral

   tokIsBlocking :: TokBlockingStyle a -> Bool
   tokIsBlocking = \case
     TokBlocking    -> True
     TokNonBlocking -> False

   reqIdsTrace :: Ack -> Req -> TokBlockingStyle a -> NodeToNodeSubmissionTrace
   reqIdsTrace ack req = \case
      TokBlocking    -> ReqIdsBlocking ack req
      TokNonBlocking -> ReqIdsPrompt   ack req

   idListTrace :: ToAnnce tx -> TokBlockingStyle a -> NodeToNodeSubmissionTrace
   idListTrace (ToAnnce (length -> toAnn)) = \case
      TokBlocking    -> IdsListBlocking toAnn
      TokNonBlocking -> IdsListPrompt   toAnn
