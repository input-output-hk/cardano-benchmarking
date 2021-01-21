{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition
  (
    mangleLocalProtocolDefinition
  ) where

import           Prelude (error)
import           Cardano.Prelude hiding (TypeError, show)

import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Config
                   ( configBlock, configCodec)
import           Ouroboros.Consensus.Config.SupportsNode
                   (ConfigSupportsNode(..), getNetworkMagic)
import           Ouroboros.Consensus.Node.ProtocolInfo
                   (ProtocolInfo (..))
import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.Chain.Slotting
import           Cardano.Api
import           Cardano.Api.Typed
import qualified Cardano.Api.Typed as Api

-- Node imports
import           Cardano.Node.Types (SocketPath(..))
import           Cardano.Tracing.OrphanInstances.Byron()
import           Cardano.Tracing.OrphanInstances.Common()
import           Cardano.Tracing.OrphanInstances.Consensus()
import           Cardano.Tracing.OrphanInstances.Network()
import           Cardano.Tracing.OrphanInstances.Shelley()

import Cardano.Benchmarking.GeneratorTx.Benchmark
import Cardano.Benchmarking.GeneratorTx.Era
import Cardano.Benchmarking.GeneratorTx.NodeToNode
import Cardano.Benchmarking.GeneratorTx
import Cardano.Benchmarking.GeneratorTx.Genesis (GeneratorFunds)

type Funding era = Benchmark -> GeneratorFunds -> ExceptT TxGenError IO (SigningKey PaymentKey, [(TxIn, TxOut era)])
type BenchmarkAction era
      = Benchmark -> (SigningKey PaymentKey, [(TxIn, TxOut era)]) -> ExceptT TxGenError IO ()

type Action era = Proxy era -> Benchmark -> GeneratorFunds -> ExceptT TxGenError IO ()

mangleLocalProtocolDefinition
  :: forall blok ptcl era.
     (
       IsShelleyBasedEra era
     , ConfigSupportsTxGen CardanoMode era
     )
  =>  Consensus.Protocol IO blok ptcl
  -> Maybe NetworkMagic
  -> Bool
  -> IOManager
  -> SocketPath
  -> BenchTracers IO CardanoBlock
  -> Action era
mangleLocalProtocolDefinition ptcl@(Consensus.ProtocolCardano
             _
             Consensus.ProtocolParamsShelleyBased{Consensus.shelleyBasedGenesis}
              _ _ _ _ _ _)
            nmagic_opt is_addr_mn iom (SocketPath sock) tracers
    = action
  where
    action _proxy benchmark fundOptions
      =  funding benchmark fundOptions >>= benchmarkAction benchmark

    ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl
    localConnectInfo = LocalNodeConnectInfo
       sock
       (Api.Testnet . getNetworkMagic . configBlock $ pInfoConfig)
       (CardanoMode (EpochSlots 21600))        -- TODO: get this from genesis

    connectClient :: ConnectClient
    connectClient  = benchmarkConnectTxSubmit
                       iom
                       (btConnect_ tracers)
                       (btSubmission_ tracers)
                       (configCodec pInfoConfig)
                       (getNetworkMagic $ configBlock pInfoConfig)

    funding :: Funding era
    funding = secureFunds
                (btTxSubmit_ tracers)
                localConnectInfo
                networkId
                shelleyBasedGenesis

    benchmarkAction :: BenchmarkAction era
    benchmarkAction = runBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) networkId connectClient

    networkId = if is_addr_mn
      then Mainnet
      else Testnet $ getNetworkMagic $ configBlock pInfoConfig

mangleLocalProtocolDefinition _ _ _ _ _ _ = error "mkCallbacks"
