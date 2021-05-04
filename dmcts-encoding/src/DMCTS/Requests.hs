{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMCTS.Requests where

import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS

import System.Random

import Control.Concurrent.Async

import DMCTS.Types
import DMCTS.SearchTree
import DMCTS.Random

import qualified Network.HTTP.Client as Net
import Network.HTTP.Simple

-- DMCTS Requests #############################################################

-- Builds a DMCTS Request from a seed and parameters specified.
buildDMCTSRequest ::
  (WeightAble l a w) =>
    SampMethodE ->
    l ->
    a ->
    Int ->
    StdGen ->
    (DMCTSRequest a, StdGen)
buildDMCTSRequest method logic root nSamp gen = (DMCTSRequest {..}, newGen)
  where
    (randSeed, newGen) = random gen:: (Int, StdGen)

-- Given a DMCTSRequest, executes the request and returns the DMCTSResponse
execDMCTSRequest :: (WeightAble l a w) => l -> DMCTSRequest a -> DMCTSResponse w

-- Leaf handler
execDMCTSRequest logic DMCTSRequest {method=SampLeaf, ..} = response
  where
    -- Build our WeightTree
    tree = mkWeightTree logic root

    -- Build our random numbers.
    gen = mkStdGen randSeed
    -- Use our rand generator to generate a set of starting StdGens
    (seeds, newGen) = getNGens gen nSamp

    -- Collect our samples
    (samps, _) = unzip $ zipWith3 sampLeaf (repeat logic) (repeat tree) seeds

    -- Aggregate the responses.
    response = DMCTSResponse {result = aggregate logic samps}

-- Spine handler
execDMCTSRequest logic DMCTSRequest {method=SampSpine, ..} = response
  where
    -- Build our WeightTree
    tree = mkWeightTree logic root

    -- Build our random numbers.
    gen = mkStdGen randSeed

    -- Use our rand generator to generate a set of starting StdGens
    (seeds, newGen) = getNGens gen nSamp

    -- Collect our samples
    (sampsList, _) = unzip $ zipWith3 sampSpine (repeat logic) (repeat tree) seeds
    samps = concat sampsList

    -- Aggregate the responses.
    response = DMCTSResponse {result = aggregate logic samps}

-- Builds and executes a DMCTS request locally
execDMCTSLocal ::
  (WeightAble l a w) =>
    SampMethodE ->
    l ->
    a ->
    Int ->
    StdGen ->
    (w, StdGen)

execDMCTSLocal method logic root nSamp gen = (res, newGen)
  where
    (req, newGen) = buildDMCTSRequest method logic root nSamp gen
    res = result $ execDMCTSRequest logic req

-- HTTP Requests #############################################################

execDMCTSRemote ::
  forall l a w.
    (WeightAble l a w,
     ToJSON (DMCTSRequest a),
     FromJSON (DMCTSResponse w)) =>
       SampMethodE ->
       DMCTSClientState ->
       l ->
       a ->
       Int ->
       StdGen ->
       IO (w, StdGen)
execDMCTSRemote method cs logic root nSamps gen = do
  res <- runAndAggHTTPRequests cs logic reqs :: IO w
  return (res, newGen)
  where
    (reqs, newGen) = buildHTTPRequests cs method logic root nSamps gen

-- Given client state and information to build a DMCTSRequest, builds an HTTP
-- request to be sent to an execution lambda.
buildHTTPRequest ::
  (WeightAble l a w, ToJSON (DMCTSRequest a)) =>
    DMCTSClientState ->
    SampMethodE ->
    l -> -- Logic
    a -> -- Label
    Int -> --nSamps
    StdGen -> --StdGen to generate the seed.
    (Net.Request, StdGen)

buildHTTPRequest cState method _ root nSamp gen = (httpReq, newGen)
  where
    (randSeed, newGen) = random gen :: (Int, StdGen)
    dmctsRequest = DMCTSRequest{..}
    -- TODO: Currently causes a runtime error if the URL isn't properly formatted.
    --       Eventually, offload this to ClientState and provide a base lambda
    --       request guaranteed to be valid.
    initReq = parseRequest_ $ "POST " ++ url (lambdaConfig cState) :: Request
    httpReq = setRequestBodyJSON dmctsRequest initReq

-- Given client state and information to build DMCTSRequests, builds several
-- HTTP requests to be sent to an execution lambda, balancing the per-lambda
-- sample number.
buildHTTPRequests ::
  (WeightAble l a w, ToJSON (DMCTSRequest a)) =>
    DMCTSClientState ->
    SampMethodE ->
    l -> -- Logic
    a -> -- Label
    Int -> --nSamps (total)
    StdGen -> --StdGen to generate the seeds.
    ([Net.Request], StdGen)

buildHTTPRequests cState method logic root nSamp gen = (reqs, newGen)
  where
    -- The number of samples to run on a specific lambda.
    perLambdaSamps = sampsPerLambda $ lambdaConfig cState
    -- The number of max-sample lambdas we need.
    wholeLambdas = quot nSamp perLambdaSamps
    -- The remaining samples to be made up for by an additional lambda.
    remainder = rem nSamp perLambdaSamps

    wholeLambdaReq = buildHTTPRequest cState method logic root perLambdaSamps
    remLambdaReq = buildHTTPRequest cState method logic root remainder

    -- Generate the seeds.
    (seeds, nextGen) = getNGens gen wholeLambdas
    (remSeed, newGen) = split nextGen

    (reqs, _) = if remainder > 0
                   then unzip $ map wholeLambdaReq seeds ++ [remLambdaReq remSeed]
                   else unzip $ map wholeLambdaReq seeds

-- Run a single HTTPRequest that's expected to return a DMCTSResponse. Currently,
-- if the request fails in any way, causes a runtime error.
--
-- TODO: Add retries, make runtime failures more graceful (either better messaging,
--       or the addition of a MaybeT transformer).
runHTTPRequest ::
  forall l a w.
    (WeightAble l a w, FromJSON (DMCTSResponse w)) =>
      DMCTSClientState ->
      l ->
      Net.Request ->
      IO (DMCTSResponse w)

runHTTPRequest cs logic req = do
  bsResponse <- httpLBS req 
  -- TODO: Use retry to give every request more than one chance. Also modify
  --       fromJust code to provide a nicer error message.
  -- NOTE: Is it better to handle a failure here, or to aggregate all the calls
  --       and account for failures in runHTTPRequests, through the MaybeT
  --       transformer? It seems easiest to handle here, but I'm not sure if
  --       that's the best pattern.
  let bsBody = getResponseBody bsResponse
      response' = decode bsBody
      errMsg = "Can not parse body: " ++ BS.unpack bsBody
      response =
        fromMaybe
          (error errMsg)
          response'
  return response


-- Given a list of HTTP requests that are expected to return DMCTSResponses, run
-- the requests and aggregate the responses.
runAndAggHTTPRequests ::
  forall l a w.
    (WeightAble l a w, FromJSON (DMCTSResponse w)) =>
      DMCTSClientState ->
      l ->
      [Net.Request]
      -> IO w

runAndAggHTTPRequests cs logic reqs = do
  responses <- mapConcurrently curriedReqRunner reqs
  let weights = map result responses :: [w]
  return $ aggregate logic weights
  where
    curriedReqRunner = runHTTPRequest cs logic
