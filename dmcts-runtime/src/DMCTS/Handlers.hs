module DMCTS.Handlers where

import Aws.Lambda

import Data.Maybe
import Data.Either

import System.Random

import DMCTS.PersonEnc
import DMCTS.Types

dmctsGatewayHandler ::
  (WeightAble logic label weight) =>
  logic ->
  ApiGatewayRequest (DMCTSRequest label) ->
  Context () ->
  IO (Either (ApiGatewayResponse String) (ApiGatewayResponse (DMCTSResponse label weight)))
dmctsGatewayHandler logic req ctx
  | isNothing body = return emptBodyResp
  | otherwise      = return succParseResp
  where
    body = apiGatewayRequestBody req
    justBody = fromJust body
    -- Sent as a response to calls with empty bodies
    emptBodyResp = Left $ mkApiGatewayResponse 400 [] "Empty Body"
    -- NOTE:
    succParseResp =
      either
        (Left . mkApiGatewayResponse 200 [])
        (Right . mkApiGatewayResponse 200 [])
        (dmctsHandler logic ctx justBody)

dmctsHandler ::
  (WeightAble logic label weight) =>
  logic ->
  Context () ->
  DMCTSRequest label ->
  Either String (DMCTSResponse label weight)

-- Handler just calls down to execRequest
dmctsHandler logic _ req
    = Left "todo: implement"


dummyGatewayHandler ::
  ApiGatewayRequest Person ->
  Context () ->
  IO (Either (ApiGatewayResponse String) (ApiGatewayResponse Person))
dummyGatewayHandler req ctx
  | isNothing body = return emptBodyResp
  | otherwise      = return succParseResp
  where
    body = apiGatewayRequestBody req
    justBody = fromJust body
    -- Sent as a response to calls with empty bodies
    emptBodyResp = Left $ mkApiGatewayResponse 400 [] "Empty Body"
    -- NOTE:
    succParseResp =
      either
        (Left . mkApiGatewayResponse 200 [])
        (Right . mkApiGatewayResponse 200 [])
        (dummyHandler ctx justBody)

dummyHandler :: Context () -> Person -> Either String Person
dummyHandler _ person =
  if personAge person > 0 then
    Right person
  else
    Left "A person's age must be positive"
