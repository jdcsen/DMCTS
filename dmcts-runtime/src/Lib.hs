module Lib where

import Aws.Lambda
import PersonEnc

handler :: Person -> Context () -> IO (Either String Person)
handler person context =
  if personAge person > 0 then
    return (Right person)
  else
    return (Left "A person's age must be positive")
