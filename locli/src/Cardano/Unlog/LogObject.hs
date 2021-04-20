{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-orphans #-}

module Cardano.Unlog.LogObject (module Cardano.Unlog.LogObject) where

import           Prelude (error)
import           Cardano.Prelude hiding (Text)

import           Control.Monad (fail)
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object, (.:))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Short as Text
import           Data.Text.Short (ShortText, fromText, toText)
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Map as Map
import           Data.Vector (Vector)

import           Cardano.BM.Stats.Resources


type Text = ShortText

readLogObjectStream :: JsonLogfile -> IO [LogObject]
readLogObjectStream (JsonLogfile f) =
  LBS.readFile f
    <&> catMaybes . fmap AE.decode . LBS.split (fromIntegral $ fromEnum '\n')

newtype JsonRunMetafile
  = JsonRunMetafile { unJsonRunMetafile :: FilePath }
  deriving (Show, Eq)

newtype JsonGenesisFile
  = JsonGenesisFile { unJsonGenesisFile :: FilePath }
  deriving (Show, Eq)

newtype JsonLogfile
  = JsonLogfile { unJsonLogfile :: FilePath }
  deriving (Show, Eq)

newtype JsonOutputFile
  = JsonOutputFile { unJsonOutputFile :: FilePath }
  deriving (Show, Eq)

newtype TextOutputFile
  = TextOutputFile { unTextOutputFile :: FilePath }
  deriving (Show, Eq)

newtype CsvOutputFile
  = CsvOutputFile { unCsvOutputFile :: FilePath }
  deriving (Show, Eq)

newtype OutputFile
  = OutputFile { unOutputFile :: FilePath }
  deriving (Show, Eq)

data LogObject
  = LogObject
    { loAt   :: !UTCTime
    , loKind :: !Text
    , loBody :: !LOBody
    }
  deriving (Generic, Show)

instance ToJSON LogObject

instance ToJSON ShortText where
  toJSON = String . toText

instance FromJSON ShortText where
  parseJSON = AE.withText "String" $ pure . fromText

instance Print ShortText where
  hPutStr   h = hPutStr   h . toText
  hPutStrLn h = hPutStrLn h . toText

newtype TId = TId { unTId :: ShortText }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

--
-- LogObject stream interpretation
--

interpreters :: Map Text (Object -> TId -> Parser LOBody)
interpreters = Map.fromList
  [ (,) "TraceStartLeadershipCheck" $
    \v _ -> LOTraceStartLeadershipCheck
            <$> v .: "slot"
            <*> v .: "utxoSize"
            <*> v .: "chainDensity"

  , (,) "TraceBlockContext" $
    \v _ -> LOBlockContext
            <$> v .: "tipBlockNo"

  , (,) "TraceNodeIsLeader" $
    \v _ -> LOTraceNodeIsLeader
            <$> v .: "slot"

  , (,) "TraceNodeNotLeader" $
    \v _ -> LOTraceNodeNotLeader
            <$> v .: "slot"

  , (,) "TraceMempoolAddedTx" $
    \v _ -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,) "TraceMempoolRemoveTxs" $
    \v _ -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,) "TraceMempoolRejectedTx" $
    \_ _ -> pure LOMempoolRejectedTx

  , (,) "TraceLedgerEvent.TookSnapshot" $
    \_ _ -> pure LOLedgerTookSnapshot

  , (,) "TraceBenchTxSubSummary" $
    \v _ -> do
       x :: Object <- v .: "summary"
       LOGeneratorSummary
         <$> ((x .: "ssFailures" :: Parser [Text])
              <&> null)
         <*> x .: "ssTxSent"
         <*> x .: "ssElapsed"
         <*> x .: "ssThreadwiseTps"

  , (,) "TraceBenchTxSubServAck" $
    \v _ -> LOTxsAcked <$> v .: "txIds"

  , (,) "Resources" $
    \v _ -> LOResources <$> parsePartialResourceStates (Object v)

  , (,) "TraceTxSubmissionCollected" $
    \v tid -> LOTxsCollected
            <$> pure tid
            <*> v .: "count"

  , (,) "TraceTxSubmissionProcessed" $
    \v tid -> LOTxsProcessed
            <$> pure tid
            <*> v .: "accepted"
            <*> v .: "rejected"
  ]

logObjectStreamInterpreterKeys :: [Text]
logObjectStreamInterpreterKeys = Map.keys interpreters

data LOBody
  = LOTraceStartLeadershipCheck !Word64 !Word64 !Float
  | LOTraceNodeIsLeader !Word64
  | LOTraceNodeNotLeader !Word64
  | LOResources !ResourceStats
  | LOMempoolTxs !Word64
  | LOMempoolRejectedTx
  | LOLedgerTookSnapshot
  | LOBlockContext !Word64
  | LOGeneratorSummary !Bool !Word64 !NominalDiffTime (Vector Float)
  | LOTxsAcked !(Vector Text)
  | LOTxsCollected !TId !Word64
  | LOTxsProcessed !TId !Word64 !Int
  | LOAny !Object
  deriving (Generic, Show)

instance ToJSON LOBody

instance FromJSON LogObject where
  parseJSON = AE.withObject "LogObject" $ \v -> do
    body :: Object <- v .: "data"
    tid  :: TId <- v .: "thread"
    -- XXX:  fix node causing the need for this workaround
    (,) unwrapped kind <- unwrap "credentials" "val" body
    LogObject
      <$> v .: "at"
      <*> pure kind
      <*> case Map.lookup kind interpreters of
            Just interp -> interp unwrapped tid
            Nothing -> pure $ LOAny unwrapped
   where
     unwrap :: Text -> Text -> Object -> Parser (Object, Text)
     unwrap wrappedKeyPred unwrapKey v = do
       kind <- (fromText <$>) <$> v AE..:? "kind"
       wrapped   :: Maybe Text <-
         (fromText <$>) <$> v AE..:? toText wrappedKeyPred
       unwrapped :: Maybe Object <- v AE..:? toText unwrapKey
       case (kind, wrapped, unwrapped) of
         (Nothing, Just _, Just x) -> (,) <$> pure x <*> (fromText <$> x .: "kind")
         (Just kind0, _, _) -> pure (v, kind0)
         _ -> fail $ "Unexpected LogObject .data: " <> show v

extendObject :: Text -> Value -> Value -> Value
extendObject k v (Object hm) = Object $ hm <> HM.singleton (toText k) v
extendObject k _ _ = error . Text.unpack $ "Summary key '" <> k <> "' does not serialise to an Object."

parsePartialResourceStates :: Value -> Parser (Resources Word64)
parsePartialResourceStates =
  AE.withObject "NodeSetup" $
    \o ->
      Resources
      <$> o .: "CentiCpu"
      <*> o .: "CentiGC"
      <*> o .: "CentiMut"
      <*> o .: "GcsMajor"
      <*> o .: "GcsMinor"
      <*> o .: "Alloc"
      <*> o .: "Live"
      <*> (o AE..:? "Heap" & fmap (fromMaybe 0))
      <*> o .: "RSS"
      <*> o .: "CentiBlkIO"
      <*> o .: "Threads"
