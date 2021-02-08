{-# LANGUAGE DeriveGeneric #-}
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

--
-- LogObject stream interpretation
--

interpreters :: Map Text (Object -> Parser LOBody)
interpreters = Map.fromList
  [ (,) "TraceStartLeadershipCheck" $
    \v -> LOTraceStartLeadershipCheck
            <$> v .: "slot"
            <*> v .: "utxoSize"
            <*> v .: "chainDensity"

  , (,) "TraceBlockContext" $
    \v -> LOBlockContext
            <$> v .: "tipBlockNo"

  , (,) "TraceNodeIsLeader" $
    \v -> LOTraceNodeIsLeader
            <$> v .: "slot"

  , (,) "TraceNodeNotLeader" $
    \v -> LOTraceNodeNotLeader
            <$> v .: "slot"

  , (,) "TraceMempoolAddedTx" $
    \v -> do x :: Object <- v .: "mempoolSize"
             LOMempoolTxs <$> x .: "numTxs"

  , (,) "TraceMempoolRemoveTxs" $
    \v -> do x :: Object <- v .: "mempoolSize"
             LOMempoolTxs <$> x .: "numTxs"

  , (,) "TraceMempoolRejectedTx" $
    \_ -> pure LOMempoolRejectedTx

  , (,) "TraceLedgerEvent.TookSnapshot" $
    \_ -> pure LOLedgerTookSnapshot

  , (,) "TraceBenchTxSubSummary" $
    \v -> do x :: Object <- v .: "summary"
             LOGeneratorSummary
               <$> ((x .: "ssFailures" :: Parser [Text])
                    <&> null)
               <*> x .: "ssTxSent"
               <*> x .: "ssElapsed"
               <*> x .: "ssThreadwiseTps"

  , (,) "TraceBenchTxSubServAck" $
    \v -> LOTxsAcked <$> v .: "txIds"

  , (,) "Resources" $
    \v -> LOResources <$> parseJSON (Object v)

  , (,) "LogValue" $
    \v -> do
      name :: ShortText <- v .: "name"
      value :: Object <- v .: "value"
      case name of
        "submissions.submitted.count" -> LOTxsSubmitted <$> value .: "contents"
        "submissions.accepted.count"  -> LOTxsAccepted  <$> value .: "contents"
        "submissions.rejected.count"  -> LOTxsRejected  <$> value .: "contents"
        _ -> fail $ "unused metric: " <> Text.unpack name
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
  | LOTxsSubmitted !Word64
  | LOTxsAccepted !Word64
  | LOTxsRejected !Word64
  | LOAny !Object
  deriving (Generic, Show)

instance ToJSON LOBody

instance FromJSON LOBody where
  parseJSON = AE.withObject "LOBody" $ \v -> do
    kind :: Text <- fromText <$> v .: "kind"
    case Map.lookup kind interpreters of
      Just interp -> interp v
      Nothing -> pure $ LOAny v

instance FromJSON LogObject where
  parseJSON = AE.withObject "LogObject" $ \v -> do
    body :: Object <- v .: "data"
    -- XXX:  fix node causing the need for this workaround
    (,) unwrapped kind <- unwrap "credentials" "val" body
    LogObject
      <$> v .: "at"
      <*> pure kind
      <*> parseJSON (extendObject "kind" (String $ toText kind) (Object unwrapped))
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
