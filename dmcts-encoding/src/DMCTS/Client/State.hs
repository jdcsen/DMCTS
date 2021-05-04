{-# LANGUAGE FlexibleContexts #-}

module DMCTS.Client.State where

import Data.Aeson

import DMCTS.Types

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Control.Monad.Trans.Maybe
import Control.Monad

-- Initializes client state, reading the Lambda config from the specified file.
newClientState :: FilePath -> MaybeT IO DMCTSClientState
newClientState lambdaCfgPath = MaybeT $ do
  lambdaConfig <- loadConfig lambdaCfgPath
  httpManager  <- newManager tlsManagerSettings
  return $ pure DMCTSClientState `ap` lambdaConfig `ap` Just httpManager

-- Loads a configuration from a file.
-- NOTE: As it stands, just a simple wrapper around the Aeson decodeFileStrict
loadConfig :: FilePath -> IO (Maybe DMCTSLambdaConfig)
loadConfig = decodeFileStrict
