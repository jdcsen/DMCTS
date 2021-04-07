module Main where

import Aws.Lambda
import Lib
import qualified Lib

main :: IO ()
main = do
  let options = defaultDispatcherOptions
  runLambdaHaskellRuntime
    options
    (pure ())
    id $ do
      addAPIGatewayHandler "handler" gatewayHandler

