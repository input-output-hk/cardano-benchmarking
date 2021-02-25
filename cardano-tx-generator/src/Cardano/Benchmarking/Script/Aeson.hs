{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Cardano.Benchmarking.Script.Aeson
where

import           Prelude

import           Data.Functor.Identity
import           Data.Text as Text
import qualified Data.HashMap.Strict as HashMap (toList)
import           Data.Aeson
import           Data.Aeson.Types (Pair, Parser, parseEither)

import           Cardano.Api (AnyCardanoEra(..))

import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters

instance ToJSON AnyCardanoEra where
  toEncoding = error "instance ToJSON AnyCardanoEra"
  toJSON = error "instance ToJSON AnyCardanoEra"
instance FromJSON AnyCardanoEra where
  parseJSON = error "fromJSON"

instance ToJSON Sum where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Sum

actionToJSON :: Action -> Value
actionToJSON a = case a of
  Set keyVal -> keyValToJSONCompact keyVal -- Remove the inner/ nested Object and add "set" -prefix.

--  other      -> genericToJSON defaultOptions other

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

jsonToAction :: Value -> Parser Action
jsonToAction
  = withObject "Error: Action is not a JSON object." $ \obj ->
  case HashMap.toList obj of
    [pair] -> parse pair
    _ -> fail "Error: cannot parse action Object."
  where
    parse :: Pair -> Parser Action
    parse (k, v) = case k of
      (Text.stripPrefix "set" -> Just tag) -> do
          s <- parseJSON $ object [ "tag" .= ("S" <> tag), "contents" .= v]
          return $ Set $ sumToTaggged s
      _ -> fail "Error: unknown key in Action object."

instance FromJSON Action where parseJSON = jsonToAction
