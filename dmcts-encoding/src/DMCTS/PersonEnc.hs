module DMCTS.PersonEnc where

import GHC.Generics
import Data.Aeson

data Person = Person
  { personName :: String
  , personAge :: Int
  } deriving (Generic)

instance FromJSON Person
instance ToJSON Person
