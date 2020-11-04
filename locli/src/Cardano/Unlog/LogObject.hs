{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Cardano.Unlog.LogObject
  ( JsonLogfile (..)
  , EpsOutputFile (..)
  , JsonOutputFile (..)
  , TextOutputFile (..)
  , LogObject (..)
  , LOBody (..)
  , readLogObjectStream
  , logObjectStreamInterpreterKeys
  , extendObject
  ) where

import           Prelude (error)
import           Cardano.Prelude

import           Control.Monad (fail)
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object, (.:))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import qualified Data.Map as Map
import           Data.Map (Map)

import           Cardano.BM.Stats.Resources


readLogObjectStream :: JsonLogfile -> IO [LogObject]
readLogObjectStream (JsonLogfile f) =
  LBS.readFile f
    <&> catMaybes . fmap AE.decode . LBS.split (fromIntegral $ fromEnum '\n')

newtype JsonLogfile
  = JsonLogfile { unJsonLogfile :: FilePath }
  deriving (Show, Eq)

newtype JsonOutputFile
  = JsonOutputFile { unJsonOutputFile :: FilePath }
  deriving (Show, Eq)

newtype TextOutputFile
  = TextOutputFile { unTextOutputFile :: FilePath }
  deriving (Show, Eq)

newtype EpsOutputFile
  = EpsOutputFile { unEpsOutputFile :: FilePath }
  deriving (Show, Eq)

data LogObject
  = LogObject
    { loAt   :: !UTCTime
    , loKind :: !Text
    , loBody :: !LOBody
    }
  deriving (Generic, Show)

instance ToJSON LogObject

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

  , (,) "TraceNodeIsLeader" $
    \v -> LOTraceNodeIsLeader
            <$> v .: "slot"

  , (,) "TraceMempoolAddedTx" $
    \v -> do mps :: Object <- v .: "mempoolSize"
             LOMempoolTxs <$> mps .: "numTxs"

  , (,) "TraceMempoolRemoveTxs" $
    \v -> do mps :: Object <- v .: "mempoolSize"
             LOMempoolTxs <$> mps .: "numTxs"

  , (,) "Resources" $
    \v -> LOResources <$> parseJSON (Object v)
  ]

logObjectStreamInterpreterKeys :: [Text]
logObjectStreamInterpreterKeys = Map.keys interpreters

data LOBody
  = LOTraceStartLeadershipCheck !Word64 !Word64 !Float
  | LOTraceNodeIsLeader !Word64
  | LOResources !ResourceStats
  | LOMempoolTxs !Word64
  | LOAny !Object
  deriving (Generic, Show)

instance ToJSON LOBody

instance FromJSON LOBody where
  parseJSON = AE.withObject "LOBody" $ \v -> do
    kind :: Text <- v .: "kind"
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
      <*> parseJSON (extendObject "kind" (String kind) (Object unwrapped))
   where
     unwrap :: Text -> Text -> Object -> Parser (Object, Text)
     unwrap wrappedKeyPred unwrapKey v = do
       kind <- v AE..:? "kind"
       wrapped   :: Maybe Text <- v AE..:? wrappedKeyPred
       unwrapped :: Maybe Object <- v AE..:? unwrapKey
       case (kind, wrapped, unwrapped) of
         (Nothing, Just _, Just x) -> (,) <$> pure x <*> (x .: "kind")
         (Just kind0, _, _) -> pure (v, kind0)
         _ -> fail $ "Unexpected LogObject .data: " <> show v

extendObject :: Text -> Value -> Value -> Value
extendObject k v (Object hm) = Object $ hm <> HM.singleton k v
extendObject k _ _ = error . Text.unpack $ "Summary key '" <> k <> "' does not serialise to an Object."
