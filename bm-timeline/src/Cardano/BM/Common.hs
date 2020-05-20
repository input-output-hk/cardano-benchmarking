{-# LANGUAGE DefaultSignatures #-}

module Cardano.BM.Common
    (
      Lineparser (..)
    , NodeId
    , SlotNum
    , Timestamp
    , remquotes
    , parseTS
    , formatTS
    , time0
    )
where

import Data.Time.Clock (UTCTime (..))
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

type NodeId = Int
type SlotNum = Int
type Timestamp = UTCTime


class Lineparser a where
    parseline :: Text -> a
    default parseline :: Text -> a
    parseline l = itemFromArray $ TL.splitOn "," l

    itemFromArray :: [Text] -> a

remquotes :: Text -> Text
remquotes t =
    let t1 = if TL.head t == '"' then TL.tail t else t
    in if TL.last t1 == '"' then TL.init t1 else t1

parseTS :: Text -> UTCTime
parseTS t =
    let tm = parseTimeM True defaultTimeLocale "%F %T%Q" $ TL.unpack t
    in case tm of
        Nothing -> error $ "failed to parse time: " ++ TL.unpack t
        Just utc -> utc

formatTS :: UTCTime -> Text
formatTS = TL.pack . formatTime defaultTimeLocale "%F %T%Q"

time0 :: UTCTime
time0 = UTCTime (toEnum 0) 0

