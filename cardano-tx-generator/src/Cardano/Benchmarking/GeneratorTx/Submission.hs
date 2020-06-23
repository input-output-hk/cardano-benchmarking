{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.GeneratorTx.Submission
  ( ROEnv(..)
  , RPCTxSubmission(..)
  , TraceBenchTxSubmit(..)
  , TraceLowLevelSubmit (..)
  , bulkSubmission
  , submitTx
  , NodeToNodeSubmissionTrace(..)
  , txSubmissionClient
  ) where

import           Prelude (error, id)
import           Cardano.Prelude hiding (ByteString, atomically, retry, threadDelay)

import           Control.Exception (assert)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadSTM (MonadSTM, TMVar, TVar,
                   atomically, newEmptyTMVarM, putTMVar, readTVar, retry,
                   takeTMVar, tryTakeTMVar)
import qualified Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime (MonadTime(..), Time, addTime,
                   diffTime, getMonotonicTime)
import           Control.Monad.Class.MonadTimer (MonadTimer, threadDelay)
import           Control.Monad.Class.MonadThrow (MonadThrow)

import           Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as A
import           Data.ByteString.Lazy (ByteString)
import           Data.List.NonEmpty (fromList)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.String (String)
import qualified Data.Text as T
import           Data.Time.Clock (DiffTime)
import           Data.Void (Void)

import           Cardano.BM.Tracing
import           Cardano.BM.Data.Tracer (emptyObject, mkObject, trStructured)
import           Control.Tracer (Tracer, traceWith)

import           Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Consensus.Byron.Ledger.Mempool as Mempool (GenTx)
import           Ouroboros.Consensus.Config (TopLevelConfig(..))
import           Ouroboros.Consensus.Ledger.SupportsMempool as Mempool
                   ( GenTxId, HasTxId, txId, txInBlockSize)
import           Ouroboros.Consensus.Network.NodeToClient
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                  (HasNetworkProtocolVersion (..), nodeToClientProtocolVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.Run (RunNode(..))

import           Ouroboros.Network.Mux
                   ( MuxMode(..), OuroborosApplication(..),
                     MuxPeer(..), RunMiniProtocol(..), RunOrStop )
import           Ouroboros.Network.Driver (runPeer)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as LocalTxSub
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))
import           Ouroboros.Network.Protocol.Handshake.Version (Versions)
import           Ouroboros.Network.Protocol.TxSubmission.Client (ClientStIdle(..),
                                                                 ClientStTxs(..),
                                                                 ClientStTxIds(..),
                                                                 TxSubmissionClient(..))
import           Ouroboros.Network.Protocol.TxSubmission.Type (BlockingReplyList(..),
                                                               TokBlockingStyle(..),
                                                               TxSizeInBytes)
import           Ouroboros.Network.NodeToClient (IOManager,
                                                 NetworkConnectTracers(..),
                                                 NodeToClientVersionData(..),
                                                 foldMapVersions,
                                                 versionedNodeToClientProtocols)
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToClient.Version (NodeToClientVersion)

import           Cardano.Config.Types (SocketPath(..))

-- | Bulk submisson of transactions.
--
--   This is intended to be used as a seperate process that interfaces
--   between some form of transaction generation mechanism
--   (e.g. benchmarking) and the local Ouroboros protocol peer. It has
--   configurable latency hiding (number of transactions to try to
--   keep ahead of actual submission) and has tracing that both
--   reports observables related to transaction submission and whether
--   the tx generation is able to keep up the pace.
--
--   For the details of the protocol description see
--   `TxSubmission` and its associated instances
--   (e.g. `Message`).
bulkSubmission
  :: forall blk txid tx .
     ( Ord txid
     , Mempool.HasTxId tx
     , RunNode blk
     , tx ~ Mempool.GenTx blk, txid ~ Mempool.GenTxId blk)
  => (ROEnv txid tx -> ROEnv txid tx)
  -- changes to default settings
  -> Tracer IO (TraceBenchTxSubmit txid)
  -> TVar IO Bool
  -- Set to True to indicate subsystem should terminate
  -> TMVar IO [tx]
  -- non-empty list of transactions to be forwarded,
  -- empty list indicates terminating
  -> TMVar IO (RPCTxSubmission IO txid tx)
  -- the RPC variable shared with
  -- `TxSubmission` local peer
  -> IO ()
bulkSubmission updEnv tr termVar txIn rpcIn =
  -- liftIO $ putStrLn "bulkSubmission__0"
  defaultRWEnv >>= evalStateT (go $ updEnv defaultROEnv)
 where
  go :: ROEnv txid tx -> StateT (RWEnv IO txid tx) IO ()
  go env = do
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "go, env: " ++ show env
    shutdown <- gets (\RWEnv{terminating, inFlight, notYetSent} ->
                       terminating &&
                       Seq.null inFlight &&
                       Seq.null notYetSent)
    if shutdown
    then do -- terminating normally, inform local peer
      -- check if remote peer is waiting on us
      -- lift . traceWith tr . TraceBenchTxSubDebug $ "TERMINATING normally"
      (opP, op) <- maybe (False, error "internal error")
                         (\x -> (True,snd x)) <$> gets availableOp
--        liftIO $ putStrLn $ "TERMINATING normally, opP: " ++ show opP
      lift . atomically $
        if opP
        then putTMVar op Nothing
        else do
          reqTxIds <- takeTMVar rpcIn -- no other message should occur as nothing is in flight
          case reqTxIds of
            RPCRequestTxIds _ resp -> putTMVar resp Nothing
            _ -> return ()
    else  -- process next interaction
      -- lift . traceWith tr . TraceBenchTxSubDebug $ "go, process next interaction"
      go1 env

  go1 :: ROEnv txid tx -> StateT (RWEnv IO txid tx) IO ()
  go1 env = do
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "go1, env: " ++ show env
    terminating' <- gets terminating
    notYetSent'  <- gets notYetSent
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "go1, terminating': " ++ show terminating'

    -- perform the blocking inquiry
    (nowTerminating, newTxs, rpc) <- lift . atomically $ do
      -- just looking for the transition to terminating here
      term' <- if terminating'
               then pure False
               else readTVar termVar
      -- try get more txs if our target backlog permits
      txs' <- if Seq.length notYetSent' < targetBacklog env &&
                 not terminating'
              then tryTakeTMVar txIn
              else pure Nothing
      -- always willing to interact with local peer
      rpc' <- tryTakeTMVar rpcIn
      -- check there was some change to process
      unless (term' || isJust txs' || isJust rpc') retry
      -- treat empty input transaction list as equiv to
      -- terminating
      let (term,txs) =
            case txs' of
              Just x | null x
                       -> (True, Nothing)
              _        -> (term', txs')
      pure (term || terminating', txs, rpc')

    -- Update terminating, if needed
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "go1, update terminating, if needed"
    when (nowTerminating && not terminating') $
      -- lift . traceWith tr . TraceBenchTxSubDebug $ "go1, update terminating: True"
      modify (\x -> x {terminating = True})

    -- incorporate new transactions from the generator into
    -- NotYetSent
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "go1, incorporate new transactions from the generator into NotYetSent"
    flip (maybe (pure ())) newTxs $ \xs -> do
      let newTxs' = map (\x -> (getTxId x, x, getTxSize x)) xs
      -- lift . traceWith tr . TraceBenchTxSubDebug $ "go1, newTxs' size: " ++ show (length newTxs')
      lift . traceWith tr . TraceBenchTxSubRecv $ map (\(a,_,_) -> a) newTxs'
      modify (\x -> x {notYetSent = notYetSent x Seq.>< Seq.fromList newTxs'})

    -- if remote peer is waiting on us and there is something to be
    -- sent: reply to the outstanding operation.
    opP <- isJust <$> gets availableOp
    haveStuffP <- (not . Seq.null) <$> gets notYetSent
    when (opP && haveStuffP) $ processOp env

    -- process any interaction with the local peer
    flip (maybe (pure ())) rpc $ processRPC env

    -- and recurse
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "go, recurse"
    go env

  getTxId :: tx -> txid
  getTxId = Mempool.txId

  getTxSize :: tx -> TxSizeInBytes
  getTxSize = txInBlockSize

  processOp :: ROEnv txid tx -> StateT (RWEnv IO txid tx) IO ()
  processOp env = do
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "processOp, env: " ++ show env
    (window, op) <- (maybe (error "internal error")) id <$> gets availableOp
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "processOp, window: " ++ show window
    (send,store) <- Seq.splitAt window <$> gets notYetSent
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "processOp, send size: " ++ show (length send)
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "processOp, store size: " ++ show (length store)
    let send' = toList send
    checkRateLimiter
    lift . traceWith tr . TraceBenchTxSubStart $ [a | (a,_,_) <- send']
    lift . atomically $ putTMVar op (Just $ [(a,c) | (a,_,c) <-  send'])
    setRateLimiter env [ c | (_,_,c) <- send']
    modify (\x -> x { availableOp = Nothing
                    , inFlight    = inFlight x Seq.>< send
                    , notYetSent  = store })
    -- lift . traceWith tr . TraceBenchTxSubDebug $ "processOp, done."

  processRPC :: ROEnv txid tx -> RPCTxSubmission IO txid tx -> StateT (RWEnv IO txid tx) IO ()
  processRPC env =
    \case
      RPCRequestTxs  txids resp -> do
        -- lift . traceWith tr . TraceBenchTxSubDebug $ "processRPC, RPCRequestTxs"
        lift . traceWith tr $ TraceBenchTxSubServReq txids
        let txset = Set.fromList txids
        result <- Seq.filter (\(x,_,_) -> Set.member x txset) <$> gets inFlight
        lift . atomically $ putTMVar resp [b |(_,b,_) <- toList result]

      -- if blocking (deferred response) then the window has to be
      -- greater than zero, otherwise you can never respond (as the
      -- list has to be non empty)
      RPCRequestTxIds (acked, window) resp -> assert (window > 0) $ do
      -- prompt reply NOT expected (blocking)
        -- lift . traceWith tr . TraceBenchTxSubDebug $ "processRPC, RPCRequestTxIds"
        haveWork <- (not . Seq.null) <$> gets notYetSent
        if haveWork
        then do
          checkRateLimiter
          (send,store) <- Seq.splitAt (fromIntegral window) <$> gets notYetSent
          let send' = toList send
          lift . traceWith tr . TraceBenchTxSubStart $ [a | (a,_,_) <- send']
          lift . atomically $ putTMVar resp (Just $ [(a,c) | (a,_,c) <-  send'])
          setRateLimiter env [ c | (_,_,c) <- send']
          modify (\x -> x { inFlight    = inFlight x Seq.>< send
                            , notYetSent  = store })
          noteBusy
        else do
          noteIdle
          modify (\x -> x { availableOp = Just (fromIntegral window, resp)})

        -- deal with acked txs
        processAcks acked

      RPCRequestTxIdsPromptly (acked, window) resp -> do
      -- prompt reply expected
        haveWork <- (not . Seq.null) <$> gets notYetSent
        if haveWork
        then do
          checkRateLimiter
          (send,store) <- Seq.splitAt (fromIntegral window) <$> gets notYetSent
          let send' = toList send
          lift . traceWith tr . TraceBenchTxSubStart $ [a | (a,_,_) <- send']
          lift . atomically $ putTMVar resp [(a,c) | (a,_,c) <-  send']
          setRateLimiter env [ c | (_,_,c) <- send']
          modify (\x -> x { inFlight    = inFlight x Seq.>< send
                          , notYetSent  = store })
          noteBusy
        else do
          noteIdle
          lift . atomically $ putTMVar resp []
          modify (\x -> x { availableOp = Nothing })

        -- deal with acked txs
        processAcks acked

  checkRateLimiter :: StateT (RWEnv IO txid tx) IO ()
  checkRateLimiter = do
    waitUntil <- gets proceedAfter
    sleepFor <- (\x -> waitUntil `diffTime` x) <$> lift getMonotonicTime
    when (sleepFor > 0) . lift $ do
      traceWith tr $ TraceBenchTxSubRateLimit sleepFor
      threadDelay sleepFor

  setRateLimiter
    :: ROEnv txid tx
    -> [TxSizeInBytes]
    -> StateT (RWEnv IO txid tx) IO ()
  setRateLimiter env tls = do
    let txLimit   = (* fromIntegral (length tls)) <$> txNumServiceTime  env
        sizeLimit = (* fromIntegral (sum    tls)) <$> txSizeServiceTime env
        limit     = max txLimit sizeLimit
    flip (maybe (pure ())) limit $ \d -> do
      liftIO . traceWith tr . TraceBenchTxSubDebug
        $ "******* sleeping for " ++ show d
      waitUntil <- (addTime d) <$> lift getMonotonicTime
      modify (\x -> x { proceedAfter = waitUntil })

  processAcks :: Word16 -> StateT (RWEnv IO txid tx) IO ()
  processAcks acked  =  when (acked > 0) $ do
    (done, left) <- (Seq.splitAt (fromIntegral acked)) <$> gets inFlight
    lift . traceWith tr $ TraceBenchTxSubServAck [a | (a,_,_) <- toList done]
    modify (\x -> x {inFlight = left})

  noteIdle :: StateT (RWEnv IO txid tx) IO ()
  noteIdle = do
--      liftIO $ putStrLn $ "noteIdle"
    wasBusy <- (== Busy) <$> gets activityState
    when wasBusy $ do
      lift . traceWith tr $ TraceBenchTxSubIdle
--        liftIO $ putStrLn $ "noteIdle, Idle!"
      modify (\x -> x { activityState = Idle})

  noteBusy :: StateT (RWEnv IO txid tx) IO ()
  noteBusy = do -- just can't get those memory write cycles out of my head
--      liftIO $ putStrLn $ "noteBusy"
    wasIdle <- (== Idle) <$> gets activityState
    when wasIdle $
--        liftIO $ putStrLn $ "noteBusy, Busy!"
      modify (\x -> x { activityState = Busy})

data ActivityState = Idle | Busy deriving (Eq)

-- | The readonly environment
data ROEnv txid tx = ROEnv
  { targetBacklog     :: Int -- ^ how many to try to keep in back pocket, >0
  , txNumServiceTime  :: Maybe DiffTime -- ^ seconds per tx
  , txSizeServiceTime :: Maybe DiffTime -- ^ seconds per tx octet
  } deriving Show

defaultROEnv :: ROEnv txid tx
defaultROEnv = ROEnv
  { targetBacklog     = 1
  , txNumServiceTime  = Nothing
  , txSizeServiceTime = Nothing
  }

data RWEnv m txid tx = RWEnv
  { terminating     :: Bool
  , activityState   :: ActivityState
  , proceedAfter    :: Time
  , availableOp     :: Maybe (Int, TMVar m (Maybe [(txid, TxSizeInBytes)]))
  -- ^ the window and the response action
  , inFlight
  , notYetSent      :: Seq (txid, tx,  TxSizeInBytes)
  }

defaultRWEnv :: MonadTime m => m (RWEnv m txid tx)
defaultRWEnv = do
  now <- getMonotonicTime
  pure $ RWEnv
    { terminating     = False
    , activityState   = Idle
    , proceedAfter    = now
    , availableOp     = Nothing
    , inFlight        = mempty
    , notYetSent      = mempty
    }

-- | RPC interaction with `TxSubmission`
data RPCTxSubmission m txid tx
  = RPCRequestTxIdsPromptly (Word16, Word16) (TMVar m [(txid, TxSizeInBytes)])
  -- ^ Request contains the acknowledged number and the size of the
  --   open window. Response contains the list of transactions (that
  --   can be empty - see the `TxSubmission` description of
  --   `StBlockingStyle` for more details). A prompt response
  --   is expected.
  |  RPCRequestTxIds (Word16, Word16) (TMVar m (Maybe [(txid, TxSizeInBytes)]))
  -- ^ Request contains the acknowledged number and the size of the
  --   open window. Response contains the list of transactions (that
  --   can not be empty - see the `TxSubmission` description
  --   of `StBlockingStyle` for more details); `Nothing`
  --   indicates no more transaction submissions and a clean
  --   shutdown. A prompt response is not expected.
  | RPCRequestTxs [txid] (TMVar m [tx])
  -- ^ Request contains the list of transaction identifiers which are
  --   returned in the response.

-- | Tracer
data TraceBenchTxSubmit txid
  = TraceBenchTxSubRecv [txid]
  -- ^ Received from generator.
  | TraceBenchTxSubStart [txid]
  -- ^ The @txid@ has been submitted to `TxSubmission`
  --   protocol peer.
  | TraceBenchTxSubServAnn [txid]
  -- ^ Announcing txids in response for server's request.
  | TraceBenchTxSubServReq [txid]
  -- ^ Request for @tx@ recieved from `TxSubmission` protocol
  --   peer.
  | TraceBenchTxSubServAck [txid]
  -- ^ An ack (window moved over) received for these transactions.
  | TraceBenchTxSubServDrop [txid]
  -- ^ Transactions the server implicitly dropped.
  | TraceBenchTxSubServOuts [txid]
  -- ^ Transactions outstanding.
  | TraceBenchTxSubIdle
  -- ^ Remote peer requested new transasctions but none were
  --   available, generator not keeping up?
  | TraceBenchTxSubRateLimit DiffTime
  -- ^ Rate limiter bit, this much delay inserted to keep within
  --   configured rate.
  | TraceBenchTxSubDebug String
  | TraceBenchTxSubError Text
  deriving (Show)

instance ToJSON (Mempool.GenTxId ByronBlock) where
  toJSON txid = A.String (T.pack $ show txid)

instance ToObject (TraceBenchTxSubmit (Mempool.GenTxId ByronBlock)) where
  toObject MinimalVerbosity _ = emptyObject -- do not log
  toObject NormalVerbosity t =
    case t of
      TraceBenchTxSubRecv _      -> mkObject ["kind" .= A.String "TraceBenchTxSubRecv"]
      TraceBenchTxSubStart _     -> mkObject ["kind" .= A.String "TraceBenchTxSubStart"]
      TraceBenchTxSubServAnn _   -> mkObject ["kind" .= A.String "TraceBenchTxSubServAnn"]
      TraceBenchTxSubServReq _   -> mkObject ["kind" .= A.String "TraceBenchTxSubServReq"]
      TraceBenchTxSubServAck _   -> mkObject ["kind" .= A.String "TraceBenchTxSubServAck"]
      TraceBenchTxSubServDrop _  -> mkObject ["kind" .= A.String "TraceBenchTxSubServDrop"]
      TraceBenchTxSubServOuts _  -> mkObject ["kind" .= A.String "TraceBenchTxSubServOuts"]
      TraceBenchTxSubIdle        -> mkObject ["kind" .= A.String "TraceBenchTxSubIdle"]
      TraceBenchTxSubRateLimit _ -> mkObject ["kind" .= A.String "TraceBenchTxSubRateLimit"]
      TraceBenchTxSubDebug _     -> mkObject ["kind" .= A.String "TraceBenchTxSubDebug"]
      TraceBenchTxSubError _     -> mkObject ["kind" .= A.String "TraceBenchTxSubError"]
  toObject MaximalVerbosity t =
    case t of
      TraceBenchTxSubRecv txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubRecv"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubStart txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubStart"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServAnn txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServAnn"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServReq txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServReq"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServAck txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServAck"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServDrop txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServDrop"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubServOuts txIds ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubServOuts"
                 , "txIds" .= toJSON txIds
                 ]
      TraceBenchTxSubIdle ->
        mkObject [ "kind" .= A.String "TraceBenchTxSubIdle"
                 ]
      TraceBenchTxSubRateLimit limit ->
        mkObject [ "kind"  .= A.String "TraceBenchTxSubRateLimit"
                 , "limit" .= toJSON limit
                 ]
      TraceBenchTxSubDebug s ->
        mkObject [ "kind" .= A.String "TraceBenchTxSubDebug"
                 , "msg"  .= A.String (T.pack s)
                 ]
      TraceBenchTxSubError s ->
        mkObject [ "kind" .= A.String "TraceBenchTxSubError"
                 , "msg"  .= A.String s
                 ]

instance ToObject (Mempool.GenTxId ByronBlock) where
  toObject MinimalVerbosity _    = emptyObject -- do not log
  toObject NormalVerbosity _     = mkObject [ "kind" .= A.String "GenTxId"]
  toObject MaximalVerbosity txid = mkObject [ "kind" .= A.String "GenTxId"
                                            , "txId" .= toJSON txid
                                            ]

instance HasSeverityAnnotation (Mempool.GenTxId ByronBlock)

instance HasPrivacyAnnotation (Mempool.GenTxId ByronBlock)

instance Transformable Text IO (Mempool.GenTxId ByronBlock) where
  trTransformer = trStructured

instance HasSeverityAnnotation (TraceBenchTxSubmit (Mempool.GenTxId ByronBlock))

instance HasPrivacyAnnotation (TraceBenchTxSubmit (Mempool.GenTxId ByronBlock))

instance Transformable Text IO (TraceBenchTxSubmit (Mempool.GenTxId ByronBlock)) where
  -- transform to JSON Object
  trTransformer = trStructured

-- | Low-tevel tracer
data TraceLowLevelSubmit
  = TraceLowLevelSubmitting
  -- ^ Submitting transaction.
  | TraceLowLevelAccepted
  -- ^ The transaction has been accepted.
  | TraceLowLevelRejected String
  -- ^ The transaction has been rejected, with corresponding error message.
  deriving (Show)

instance ToObject TraceLowLevelSubmit where
  toObject MinimalVerbosity _ = emptyObject -- do not log
  toObject NormalVerbosity t =
    case t of
      TraceLowLevelSubmitting -> mkObject ["kind" .= A.String "TraceLowLevelSubmitting"]
      TraceLowLevelAccepted   -> mkObject ["kind" .= A.String "TraceLowLevelAccepted"]
      TraceLowLevelRejected m -> mkObject [ "kind" .= A.String "TraceLowLevelRejected"
                                          , "message" .= A.String (T.pack m)
                                          ]
  toObject MaximalVerbosity t =
    case t of
      TraceLowLevelSubmitting ->
        mkObject [ "kind" .= A.String "TraceLowLevelSubmitting"
                 ]
      TraceLowLevelAccepted ->
        mkObject [ "kind" .= A.String "TraceLowLevelAccepted"
                 ]
      TraceLowLevelRejected errMsg ->
        mkObject [ "kind"   .= A.String "TraceLowLevelRejected"
                 , "errMsg" .= A.String (T.pack errMsg)
                 ]

instance HasSeverityAnnotation TraceLowLevelSubmit

instance HasPrivacyAnnotation TraceLowLevelSubmit

instance (MonadIO m) => Transformable Text m TraceLowLevelSubmit where
  -- transform to JSON Object
  trTransformer = trStructured

{-------------------------------------------------------------------------------
  Main logic
-------------------------------------------------------------------------------}

submitTx :: ( RunNode blk )
         => IOManager
         -> SocketPath
         -> TopLevelConfig blk
         -> GenTx blk
         -> Tracer IO TraceLowLevelSubmit
         -> IO ()
submitTx iocp sockpath cfg tx tracer =
    let path = unSocketPath sockpath
    in NodeToClient.connectTo
         (NodeToClient.localSnocket iocp path)
         NetworkConnectTracers {
             nctMuxTracer       = nullTracer,
             nctHandshakeTracer = nullTracer
           }
         (localInitiatorNetworkApplication tracer cfg tx)
         path


localInitiatorNetworkApplication
  :: forall blk m .
     ( RunNode blk
     , MonadST m
     , MonadThrow m
     , MonadTimer m
     )
  => Tracer m TraceLowLevelSubmit
  -> TopLevelConfig blk
  -> GenTx blk
  -> Versions NodeToClient.NodeToClientVersion NodeToClient.DictVersion
              (OuroborosApplication InitiatorMode NodeToClient.LocalAddress ByteString m () Void)

localInitiatorNetworkApplication tracer cfg tx =
  foldMapVersions
    (\v ->
      versionedNodeToClientProtocols
        (nodeToClientProtocolVersion proxy v)
        versionData
        (protocols v))
    (supportedNodeToClientVersions proxy)
 where
    proxy :: Proxy blk
    proxy = Proxy

    versionData = NodeToClientVersionData $ getNetworkMagic $ configBlock cfg

    protocols :: BlockNodeToClientVersion blk
              -> NodeToClient.ConnectionId NodeToClient.LocalAddress
              -> Control.Monad.Class.MonadSTM.STM m RunOrStop
              -> NodeToClient.NodeToClientProtocols InitiatorMode ByteString m () Void
    protocols byronClientVersion _ _=
        NodeToClient.NodeToClientProtocols {
          NodeToClient.localChainSyncProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                nullTracer
                cChainSyncCodec
                NodeToClient.chainSyncPeerNull

        , NodeToClient.localTxSubmissionProtocol =
            InitiatorProtocolOnly $
              MuxPeerRaw $ \channel -> do
                traceWith tracer TraceLowLevelSubmitting
                (result, maybs) <- runPeer
                                     nullTracer -- (contramap show tracer)
                                     cTxSubmissionCodec
                                     channel
                                     (LocalTxSub.localTxSubmissionClientPeer
                                       (txSubmissionClientSingle tx))
                case result of
                  SubmitSuccess -> traceWith tracer TraceLowLevelAccepted
                  SubmitFail msg -> traceWith tracer (TraceLowLevelRejected $ show msg)
                return ((), maybs)

        , NodeToClient.localStateQueryProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                nullTracer
                cStateQueryCodec
                NodeToClient.localStateQueryPeerNull
        }
     where
      Codecs { cChainSyncCodec
             , cTxSubmissionCodec
             , cStateQueryCodec
             } = defaultCodecs (getCodecConfig $ configBlock cfg) byronClientVersion


-- | A 'LocalTxSubmissionClient' that submits exactly one transaction, and then
-- disconnects, returning the confirmation or rejection.
--
txSubmissionClientSingle
  :: forall tx reject m.
     Applicative m
  => tx
  -> LocalTxSub.LocalTxSubmissionClient tx reject m (LocalTxSub.SubmitResult reject)
txSubmissionClientSingle tx = LocalTxSub.LocalTxSubmissionClient $
    pure $ LocalTxSub.SendMsgSubmitTx tx $ \mreject ->
      pure (LocalTxSub.SendMsgDone mreject)

instance ToObject NodeToNodeSubmissionTrace where
  toObject MinimalVerbosity = const emptyObject -- do not log
  toObject _ = \case
    ReqIdsBlocking  ack req -> mkObject [ "kind" .= A.String "ReqIdsBlocking"
                                        , "ack"  .= A.toJSON ack
                                        , "req"  .= A.toJSON req ]
    IdsListBlocking sent    -> mkObject [ "kind" .= A.String "IdsListBlocking"
                                        , "sent" .= A.toJSON sent ]
    ReqIdsPrompt    ack req -> mkObject [ "kind" .= A.String "ReqIdsPrompt"
                                        , "ack"  .= A.toJSON ack
                                        , "req"  .= A.toJSON req ]
    IdsListPrompt   sent    -> mkObject [ "kind" .= A.String "IdsListPrompt"
                                        , "sent" .= A.toJSON sent ]
    EndOfProtocol           -> mkObject [ "kind" .= A.String "EndOfProtocol" ]
    ReqTxs          req     -> mkObject [ "kind" .= A.String "ReqTxs"
                                        , "req"  .= A.toJSON req ]
    TxList          sent    -> mkObject [ "kind" .= A.String "TxList"
                                        , "sent" .= A.toJSON sent ]

instance HasSeverityAnnotation NodeToNodeSubmissionTrace
instance HasPrivacyAnnotation  NodeToNodeSubmissionTrace
instance Transformable Text IO NodeToNodeSubmissionTrace where
  trTransformer = trStructured

data NodeToNodeSubmissionTrace
  = ReqIdsBlocking  Word16 Word16
  | IdsListBlocking Int

  | ReqIdsPrompt    Word16 Word16
  | IdsListPrompt   Int

  | ReqTxs          Int
  | TxList          Int

  | EndOfProtocol

txSubmissionClient
  :: forall m block txid tx .
     ( MonadSTM m
     , txid ~ Mempool.GenTxId block
     , tx ~ GenTx block)
  => Tracer m NodeToNodeSubmissionTrace
  -> TMVar m (RPCTxSubmission m txid tx)
  -> TxSubmissionClient txid tx m ()
txSubmissionClient tr tmvReq =
  TxSubmissionClient $ pure client
 where
  client = ClientStIdle
    { recvMsgRequestTxIds = \blocking acked window ->
        case blocking of
          TokBlocking -> do -- prompt reply not required
            traceWith tr $ ReqIdsBlocking acked window
            resp' <- newEmptyTMVarM
            atomically . putTMVar tmvReq $
              RPCRequestTxIds (acked, window) resp'
            -- might be some delay at this point
            r <- atomically $ takeTMVar resp'
            case r of
              Nothing  -> do
                traceWith tr $ EndOfProtocol
                pure $ SendMsgDone ()
              Just txs -> do
                traceWith tr $ IdsListPrompt (length txs)
                pure $ SendMsgReplyTxIds (BlockingReply $ fromList txs) client
          TokNonBlocking -> do -- prompt reply required
            traceWith tr $ ReqIdsPrompt acked window
            resp' <- newEmptyTMVarM
            atomically . putTMVar tmvReq $
              RPCRequestTxIdsPromptly (acked, window) resp'
            txs <- atomically $ takeTMVar resp'
            traceWith tr $ IdsListBlocking (length txs)
            pure $ SendMsgReplyTxIds (NonBlockingReply txs) client
    , recvMsgRequestTxs = \txids -> do
        traceWith tr $ ReqTxs (length txids)
        resp' <- newEmptyTMVarM
        atomically $ putTMVar tmvReq $ RPCRequestTxs txids resp'
        r <- atomically $ takeTMVar resp'
        traceWith tr $ TxList (length r)
        pure $ SendMsgReplyTxs r client
    , recvMsgKThxBye = return ()
    }
