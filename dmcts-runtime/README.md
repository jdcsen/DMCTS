# dmcts-runtime

The DMCTS Runtime provides functionality to evaluate and accumulate the MCTS from a specified root node. Currently, users must deploy a main function of the form:

```
module Main where

import Aws.Lambda
import DMCTS.Handlers

import -- APPLICATION SPECIFIC LOGIC --

main :: IO ()
main = do
  let options = defaultDispatcherOptions
  runLambdaHaskellRuntime
    options
    (pure ())
    id $ do
      addAPIGatewayHandler "" (dmctsGatewayHandler --NODE LOGIC--)

```

This defines the main function to be the default aws-lambda-haskell-runtime dispatcher, with a single, unnamed handler that processes DMCTSRequests and returns DMCTSResponses.

## Future Work:
It seems like a natural extension to have a main function dynamically call a gatewayHandler based upon whatever logic is specified in a DMCTSRequest, so users can define a runtime that can handle a variety of search semantics depending on how it's commanded.
