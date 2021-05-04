module Main where

import qualified System.IO as SIO

import Aws.Lambda
import DMCTS.Handlers

import DMCTSKnapsack.KnapTree

main :: IO ()
main = do
  let options = defaultDispatcherOptions
  runLambdaHaskellRuntime
    options
    (pure ())
    id $ do
      addAPIGatewayHandler "" (dmctsGatewayHandler KnapLogicNR)
