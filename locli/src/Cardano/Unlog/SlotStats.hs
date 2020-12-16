{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Unlog.SlotStats (module Cardano.Unlog.SlotStats) where

import           Cardano.Prelude

import           Data.Aeson
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

import           Data.Time.Clock (UTCTime, NominalDiffTime)
import           Text.Printf

import           Data.Accum
import           Cardano.Unlog.Resources


data SlotStats
  = SlotStats
    { slSlot        :: !Word64
    , slEpoch       :: !Word64
    , slEpochSlot   :: !Word64
    , slStart       :: !UTCTime
    , slCountChecks :: !Word64
    , slCountLeads  :: !Word64
    , slChainDBSnap :: !Word64
    , slRejectedTx  :: !Word64
    , slBlockNo     :: !Word64
    , slOrderViol   :: !Word64
    , slEarliest    :: !UTCTime
    , slSpan        :: !NominalDiffTime
    , slMempoolTxs  :: !Word64
    , slUtxoSize    :: !Word64
    , slDensity     :: !Float
    , slResources   :: !(Resources (Maybe Word64))
    }
  deriving (Generic, Show)

instance ToJSON SlotStats

-- | Initial and trailing data are noisy outliers: drop that.
--
--   The initial part is useless until the node actually starts
--   to interact with the blockchain, so we drop all slots until
--   they start getting non-zero chain density reported.
--
--   On the trailing part, we drop everything since the last leadership check.
cleanupSlotStats :: Seq SlotStats -> Seq SlotStats
cleanupSlotStats =
  Seq.dropWhileL ((== 0) . slDensity) .
  Seq.dropWhileR ((== 0) . slCountChecks)

toLeadershipLine :: Bool -> Text -> SlotStats -> Text
toLeadershipLine exportMode leadershipF SlotStats{..} = Text.pack $
  printf (Text.unpack leadershipF)
         sl epsl epo blk chks  lds cdbsn rejtx span dens cpu gc mut majg ming   pro liv alc rss atm mpo utx
 where sl    = slSlot
       epsl  = slEpochSlot
       epo   = slEpoch
       blk   = slBlockNo
       chks  = slCountChecks
       lds   = slCountLeads
       cdbsn = slChainDBSnap
       rejtx = slRejectedTx
       span  = show slSpan :: Text
       cpu   = d 3 $ rCentiCpu slResources
       dens  = slDensity
       gc    = d 2 $ rCentiGC  slResources
       mut   = d 2 $ rCentiMut slResources
       majg  = d 2 $ rGcsMajor slResources
       ming  = d 2 $ rGcsMinor slResources
       pro   = f 2 $ calcProd <$> (fromIntegral <$> rCentiMut slResources :: Maybe Float)
                              <*> (fromIntegral <$> rCentiCpu slResources)
       liv   = d 7 (rLive     slResources)
       alc   = d 7 (rAlloc    slResources)
       rss   = d 7 (rRSS      slResources)
       atm   = d 8 $
               (ceiling :: Float -> Int)
               <$> ((/) <$> (fromIntegral . (100 *) <$> rAlloc slResources)
                        <*> (fromIntegral . max 1 . (1024 *) <$> rCentiMut slResources))
       mpo   = slMempoolTxs
       utx   = slUtxoSize

       calcProd :: Float -> Float -> Float
       calcProd mut' cpu' = if cpu' == 0 then 1 else mut' / cpu'

       d, f :: PrintfArg a => Int -> Maybe a -> Text
       d width = \case
         Just x  -> Text.pack $ printf ("%"<>(if exportMode then "0" else "")
                                           <>show width<>"d") x
         Nothing -> mconcat (replicate width "-")
       f width = \case
         Just x  -> Text.pack $ printf ("%0."<>show width<>"f") x
         Nothing -> mconcat (replicate width "-")

zeroSlotStats :: SlotStats
zeroSlotStats =
  SlotStats
  { slSlot = 0
  , slEpoch = 0
  , slEpochSlot = 0
  , slStart = zeroUTCTime
  , slCountChecks = 0
  , slCountLeads = 0
  , slOrderViol = 0
  , slEarliest = zeroUTCTime
  , slSpan = realToFrac (0 :: Int)
  , slMempoolTxs = 0
  , slUtxoSize = 0
  , slDensity = 0
  , slResources = pure Nothing
  , slChainDBSnap = 0
  , slRejectedTx = 0
  , slBlockNo = 0
  }