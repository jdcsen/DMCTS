module Lib where

import Aws.Lambda
import PersonEnc
import Data.Maybe
import Data.Either

gatewayHandler ::
  ApiGatewayRequest Person ->
  Context () ->
  IO (Either (ApiGatewayResponse String) (ApiGatewayResponse Person))
gatewayHandler req ctx
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
        (handler ctx justBody)

handler :: Context () -> Person -> Either String Person
handler _ person =
  if personAge person > 0 then
    Right person
  else
    Left "A person's age must be positive"
