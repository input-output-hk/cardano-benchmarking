{-# Language LambdaCase #-}
module Cardano.Benchmarking.TxGenerator.Phase1
where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import qualified Data.List.NonEmpty as NE

import qualified Cardano.Benchmarking.TxGenerator.CLI.Parsers as P
import           Cardano.Benchmarking.TxGenerator.Error (TxGenError (..))
import           Cardano.Benchmarking.TxGenerator.Producer as Producer
import           Cardano.Benchmarking.TxGenerator.Types as T

import           Cardano.Api.Typed as Api
import           Cardano.Api.TxSubmit as Api

runPhase1 :: P.GenerateTxs -> NE.NonEmpty a0 -> ExceptT TxGenError IO [Producer]
runPhase1 args remoteAddresses = do
  key    <- readKey keyFile
  srcAddr <- readAddr addressFile
  let initialFund = Producer.Producer
         { Producer.network = thisNetwork
         , Producer.ttl  = fromInteger 10000000
         , Producer.fee  = Lovelace $ fromIntegral $ T.unFeePerTx $ P.fee args
         , Producer.addr = makeShelleyAddress
                              thisNetwork
                              (PaymentCredentialByKey $ verificationKeyHash srcAddr)
                              NoStakeAddress
         , Producer.skey = key
         , Producer.src  = utxo
         , Producer.fund = Lovelace $ fromIntegral value
         }
  (tx,p) <- withExceptT Phase1SplitError $ hoistEither $ Producer.split splitList initialFund
  newExceptT $ fmap mapSubmitError $ Api.submitTx connectInfo $ TxForShelleyMode tx
  return p
  where
    (P.InitialFund value keyFile utxo addressFile) = P.initialFund args
    thisNetwork = P.network args
    connectInfo = LocalNodeConnectInfo
      { localNodeSocketPath    = P.socketPath args
      , localNodeNetworkId     = thisNetwork
      , localNodeConsensusMode = ShelleyMode
      }

    txCount = NE.length remoteAddresses
    splitVal = Lovelace $ fromIntegral (value - (T.unFeePerTx $ P.fee args) )
                                           `div` fromIntegral txCount
    splitList :: [Lovelace]
    splitList = replicate txCount splitVal

readKey :: FilePath -> ExceptT TxGenError IO (SigningKey PaymentKey)
readKey keyFile
  = withExceptT TxFileError $ newExceptT $ readFileTextEnvelopeAnyOf fileTypes keyFile
  where
    fileTypes = [ FromSomeType (AsSigningKey AsPaymentKey) id ]

readAddr :: FilePath -> ExceptT TxGenError IO (VerificationKey PaymentKey)
readAddr keyFile
  = withExceptT TxFileError $ newExceptT $ readFileTextEnvelopeAnyOf fileTypes keyFile
  where
    fileTypes = [ FromSomeType (AsVerificationKey AsPaymentKey) id ]

mapSubmitError :: TxSubmitResultForMode ShelleyMode -> Either TxGenError ()
mapSubmitError e = case e of
      TxSubmitSuccess -> Right ()
      err             -> Left $ TxSubmitError $ show err
