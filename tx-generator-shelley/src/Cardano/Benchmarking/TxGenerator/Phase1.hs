{-# Language LambdaCase #-}
module Cardano.Benchmarking.TxGenerator.Phase1
where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Control.Monad.IO.Class
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text

import qualified Cardano.Benchmarking.TxGenerator.CLI.Parsers as P
import           Cardano.Benchmarking.TxGenerator.Error (TxGenError (..))
import           Cardano.Benchmarking.TxGenerator.Producer as Producer
import           Cardano.Benchmarking.TxGenerator.Submission
import           Cardano.Benchmarking.TxGenerator.Types as T

import           Cardano.Api as Api
import qualified Cardano.Api.Typed as TApi

runPhase1 :: P.GenerateTxs -> NE.NonEmpty a0 -> ExceptT TxGenError IO [Producer]
runPhase1 args remoteAddresses = do
  skey    <- readKey keyFile
  utxoIn  <- withExceptT UTxOParseError $ hoistEither $ parseTxIn $ Text.pack utxo
  srcAddr <- withExceptT AddrParseError $ hoistEither $ addressFromHex $ Text.pack address
  let initialFund = Producer.Producer
         { Producer.network = network
         , Producer.ttl  = SlotNo 10000000
         , Producer.fee  = Lovelace $ fromIntegral $ T.unFeePerTx $ P.fee args
         , Producer.addr = srcAddr
         , Producer.skey = skey
         , Producer.src  = utxoIn
         , Producer.fund = Lovelace $ fromIntegral value
         }
  (tx,p) <- withExceptT Phase1SplitError $ hoistEither $ Producer.split splitList initialFund
  newExceptT $ fmap mapSubmitError $ Api.submitTx network (P.socketPath args) tx
  return p
  where
    (P.InitialFund value keyFile utxo address) = P.initialFund args
    txCount = NE.length remoteAddresses
    splitVal = Lovelace $ fromIntegral (value - (T.unFeePerTx $ P.fee args) ) `div` fromIntegral txCount
    splitList :: [Lovelace]
    splitList = replicate txCount splitVal
    network = P.network args
    mapSubmitError e = case e of
      TxSubmitSuccess -> Right ()
      err             -> Left $ TxSubmitError $ show err

readKey :: FilePath -> ExceptT TxGenError IO SigningKey
readKey keyFile
  = do
    k <- withExceptT TxFileError $ newExceptT $ TApi.readFileTextEnvelopeAnyOf fileTypes keyFile
    return $ castKey k
  where
    fileTypes = [ TApi.FromSomeType (TApi.AsSigningKey TApi.AsPaymentKey) id ]
    castKey (TApi.PaymentSigningKey typedK) = SigningKeyShelley typedK
