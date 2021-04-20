
module DMCTS.Runtimes where

import Aws.Lambda

import DMCTS.Types
import DMCTS.Handlers

dummyRuntime :: IO ()
dummyRuntime = do
  let options = defaultDispatcherOptions
  runLambdaHaskellRuntime
    options
    (pure ())
    id $ do
      addAPIGatewayHandler "" dummyGatewayHandler
