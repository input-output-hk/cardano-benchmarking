{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Benchmarking.GeneratorTx.CLI.Run
  ( runCommand
  ) where

import           Data.Version
                    ( showVersion )
import           Data.Text
                    ( pack )
import           Paths_cardano_tx_generator
                    ( version )
import qualified Formatting as F
import           Cardano.Prelude hiding (option)
import           Control.Monad.Trans.Except.Extra
                    ( firstExceptT, left )

import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Network.NodeToClient
                    ( AssociateWithIOCP
                    , withIOManager
                    )

import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Update (ApplicationName(..))
import           Cardano.Config.Logging
                    ( createLoggingFeatureCLI )
import           Cardano.Config.Protocol
                    ( ProtocolInstantiationError
                    , SomeProtocol(..)
                    , fromProtocol
                    )
import           Cardano.Config.Types
                    ( GenesisFile(..)
                    , CardanoEnvironment(..), LastKnownBlockVersion(..)
                    , Protocol, SigningKeyFile(..), Update(..)
                    , ncLogMetrics, ncReqNetworkMagic, ncProtocol
                    , parseNodeConfigurationFP
                    )

import qualified Cardano.Crypto.Hashing as Crypto

import           Cardano.Benchmarking.GeneratorTx.Error
                    ( TxGenError )
import           Cardano.Benchmarking.GeneratorTx.CLI.Parsers
                    ( GenerateTxs (..) )
import           Cardano.Benchmarking.GeneratorTx
                    ( genesisBenchmarkRunner )

data RealPBFTError =
    IncorrectProtocolSpecified !Protocol
  | FromProtocolError !ProtocolInstantiationError
  | GenesisBenchmarkRunnerError !TxGenError
  deriving Show

data CliError =
    GenesisReadError !FilePath !Genesis.GenesisDataError
  | GenerateTxsError !RealPBFTError
  deriving Show

------------------------------------------------------------------------------------------------

runCommand :: GenerateTxs -> ExceptT CliError IO ()
runCommand (GenerateTxs logConfigFp
                        signingKey
                        delegCert
                        genFile
                        socketFp
                        targetNodeAddresses
                        numOfTxs
                        numOfInsPerTx
                        numOfOutsPerTx
                        feePerTx
                        tps
                        txAdditionalSize
                        explorerAPIEndpoint
                        sigKeysFiles) =
  withIOManagerE $ \iocp -> do
    -- Default update value
    let update = Update (ApplicationName "cardano-tx-generator") 1 $ LastKnownBlockVersion 0 2 0
    nc <- liftIO $ parseNodeConfigurationFP logConfigFp

    -- Logging layer
    (loggingLayer, _) <- liftIO $ createLoggingFeatureCLI
                                    (pack $ showVersion version)
                                    NoEnvironment
                                    (Just logConfigFp)
                                    (ncLogMetrics nc)

    genHash <- getGenesisHash genFile

    SomeProtocol p <- firstExceptT GenerateTxsError $
        firstExceptT FromProtocolError $ fromProtocol genHash
                                                      Nothing
                                                      Nothing
                                                      (Just genFile)
                                                      (ncReqNetworkMagic nc)
                                                      Nothing
                                                      (Just delegCert)
                                                      (Just signingKey)
                                                      update
                                                      (ncProtocol nc)
    case p of
        proto@Consensus.ProtocolRealPBFT{} -> firstExceptT GenerateTxsError $
            firstExceptT GenesisBenchmarkRunnerError $ genesisBenchmarkRunner
                                                        loggingLayer
                                                        iocp
                                                        socketFp
                                                        proto
                                                        targetNodeAddresses
                                                        numOfTxs
                                                        numOfInsPerTx
                                                        numOfOutsPerTx
                                                        feePerTx
                                                        tps
                                                        txAdditionalSize
                                                        explorerAPIEndpoint
                                                        [fp | SigningKeyFile fp <- sigKeysFiles]
        _ -> left $ GenerateTxsError $ IncorrectProtocolSpecified (ncProtocol nc)

----------------------------------------------------------------------------

getGenesisHash :: GenesisFile -> ExceptT CliError IO Text
getGenesisHash genFile = do
  (_, Genesis.GenesisHash gHash) <- readGenesis genFile
  return $ F.sformat Crypto.hashHexF gHash
 where
  -- | Read genesis from a file.
  readGenesis :: GenesisFile -> ExceptT CliError IO (Genesis.GenesisData, Genesis.GenesisHash)
  readGenesis (GenesisFile fp) = firstExceptT (GenesisReadError fp) $ Genesis.readGenesisData fp

withIOManagerE :: (AssociateWithIOCP -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)
