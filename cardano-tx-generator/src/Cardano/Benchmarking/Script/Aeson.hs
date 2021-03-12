{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Cardano.Benchmarking.Script.Aeson
where

import           Prelude

import           Data.Functor.Identity

import           Data.Text (Text)
import qualified Data.Text as Text

import           Data.Dependent.Sum
import qualified Data.HashMap.Strict as HashMap (toList, lookup)

import qualified Data.ByteString.Lazy as BSL
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.Encode.Pretty

import           Cardano.Api (AnyCardanoEra(..), CardanoEra(..))
import           Cardano.CLI.Types (SigningKeyFile(..))

import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store

testJSONRoundTrip :: [Action] -> Maybe String
testJSONRoundTrip l = case fromJSON $ toJSON l of
  Success r -> if l == r then Nothing else Just "compare: not equal"
  Error err -> Just err

prettyPrint :: [Action] -> BSL.ByteString
prettyPrint = encodePretty' conf
  where
    conf = defConfig {confCompare = keyOrder actionNames }
    actionNames :: [Text]
    actionNames =
      [ "startProtocol", "readSigningKey", "secureGenesisFund", "splitFund"
      , "splitFundToList", "delay", "prepareTxList", "runBenchmark", "asyncBenchmark"]

instance ToJSON AnyCardanoEra where
  toJSON era = case era of
    AnyCardanoEra ByronEra   -> String "ByronEra"
    AnyCardanoEra ShelleyEra -> String "ShelleyEra"
    AnyCardanoEra AllegraEra -> String "AllegraEra"
    AnyCardanoEra MaryEra    -> String "MaryEra"

instance FromJSON AnyCardanoEra where
  parseJSON = withText "AnyCardanoEra" $ \case
    "ByronEra"   -> return $ AnyCardanoEra ByronEra
    "ShelleyEra" -> return $ AnyCardanoEra ShelleyEra
    "AllegraEra" -> return $ AnyCardanoEra AllegraEra
    "MaryEra"    -> return $ AnyCardanoEra MaryEra
    era -> fail $ Text.unpack era

instance ToJSON (DSum Tag Identity) where
  toEncoding = error "DSum Tag Identity"
  toJSON = error "DSum Tag Identity"

instance FromJSON (DSum Tag Identity) where
  parseJSON = error "fromJSON"

instance ToJSON Sum where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Sum

actionToJSON :: Action -> Value
actionToJSON a = case a of
  Set keyVal -> keyValToJSONCompact keyVal -- Remove the inner/ nested Object and add "set" -prefix.
  StartProtocol filePath
    -> object ["startProtocol" .= filePath]
  ReadSigningKey (KeyName name) (SigningKeyFile filePath)
    -> object ["readSigningKey" .= name, "filePath" .= filePath]
  SecureGenesisFund (FundName fundName) (KeyName fundKey) (KeyName genesisKey)
    -> object ["secureGenesisFund" .= fundName, "fundKey" .= fundKey, "genesisKey" .= genesisKey ]
  SplitFund newFunds (KeyName newKey) (FundName sourceFund)
    -> object ["splitFund" .= names, "newKey" .= newKey, "sourceFund" .= sourceFund]
    where names = [n | FundName n <- newFunds]
  SplitFundToList (FundListName fundList) (KeyName destKey) (FundName sourceFund)
    -> object ["splitFundToList" .= fundList, "newKey" .= destKey, "sourceFund" .= sourceFund ]
  Delay
    -> object ["delay" .= Null ]
  PrepareTxList (TxListName name) (KeyName key) (FundListName fund)
    -> object ["prepareTxList" .= name, "newKey" .= key, "fundList" .= fund ]
  RunBenchmark (TxListName txs)
    -> object ["runBenchmark" .= txs]
  AsyncBenchmark (ThreadName t) (TxListName txs)
    -> object ["asyncBenchmark" .= t, "txList" .= txs]
  WaitBenchmark (ThreadName t)
    -> object ["waitBenchmark" .= t]

keyValToJSONCompact :: SetKeyVal -> Value
keyValToJSONCompact keyVal = case parseEither (withObject "internal Error" parseSum) v of
  Right c  -> c
  Left err -> error err
  where
    v = toJSON $ runIdentity $ taggedToSum keyVal
    parseSum obj = do
      key <- obj .: "tag"
      (val :: Value)  <- obj .: "contents"
      return $ object [("set" <> Text.tail key) .= val]

instance ToJSON Action where toJSON = actionToJSON
instance FromJSON Action where parseJSON = jsonToAction

jsonToAction :: Value -> Parser Action
jsonToAction = withObject "Error: Action is not a JSON object." objectToAction

objectToAction :: Object -> Parser Action
objectToAction obj = case obj of
  (HashMap.lookup "startProtocol"     -> Just v) -> parseStartProtocol v
  (HashMap.lookup "readSigningKey"    -> Just v) -> parseReadSigningKey v
  (HashMap.lookup "secureGenesisFund" -> Just v) -> parseSecureGenesisFund v
  (HashMap.lookup "splitFund"         -> Just v) -> parseSplitFund v
  (HashMap.lookup "splitFundToList"   -> Just v) -> parseSplitFundToList v
  (HashMap.lookup "delay"             -> Just v) -> parseDelay v
  (HashMap.lookup "prepareTxList"     -> Just v) -> parsePrepareTxList v
  (HashMap.lookup "runBenchmark"      -> Just v) -> parseRunBenchmark v
  (HashMap.lookup "asyncBenchmark"    -> Just v) -> parseAsyncBenchmark v
  (HashMap.lookup "waitBenchmark"     -> Just v) -> parseWaitBenchmark v
  (HashMap.toList -> [(k, v) ]                 ) -> parseSetter k v
  _ -> fail "Error: cannot parse action Object."
  where
    parseSetter t v = case t of
      (Text.stripPrefix "set" -> Just tag) -> do
          s <- parseJSON $ object [ "tag" .= ("S" <> tag), "contents" .= v]
          return $ Set $ sumToTaggged s
      _ -> fail "Failed to parse Setter"

    parseStartProtocol = withText "Error parsing startProtocol" $ \t -> return $ StartProtocol $ Text.unpack t

    parseKey f = KeyName <$> parseField obj f
    parseFund f = FundName <$> parseField obj f

    parseReadSigningKey v = ReadSigningKey
      <$> ( KeyName <$> parseJSON v )
      <*> ( SigningKeyFile <$> parseField obj "filePath" )

    parseSecureGenesisFund v = SecureGenesisFund
      <$> ( FundName <$> parseJSON v )
      <*> parseKey "fundKey"
      <*> parseKey "genesisKey"

    parseSplitFund v  = do
      l <- parseJSON v
      k <- parseKey "newKey"
      f <- parseFund "sourceFund"
      return $ SplitFund (map FundName l) k f

    parseSplitFundToList v = SplitFundToList
      <$> ( FundListName <$> parseJSON v )
      <*> parseKey "newKey"
      <*> parseFund "sourceFund"

    parseDelay Null = return Delay
    parseDelay v = typeMismatch "Delay" v

    parsePrepareTxList v = PrepareTxList
      <$> ( TxListName <$> parseJSON v )
      <*> parseKey "newKey"
      <*> ( FundListName <$>parseField obj "fundList" )

    parseRunBenchmark
      = withText "Error parsing runBenchmark" $ \t -> return $ RunBenchmark $ TxListName $ Text.unpack t

    parseWaitBenchmark
      = withText "Error parsing waitBenchmark" $ \t -> return $ WaitBenchmark $ ThreadName $ Text.unpack t

    parseAsyncBenchmark v = AsyncBenchmark
      <$> ( ThreadName <$> parseJSON v )
      <*> ( TxListName <$> parseField obj "txList" )

