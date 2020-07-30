module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Cardano.Benchmarking.MockServer as MockServer

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "direct/pure client-server connect"
    [ testCase "tx-send == tx-received" $ MockServer.test >>= assertBool "direct connect identity"
    ]
