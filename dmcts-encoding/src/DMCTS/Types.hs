{-# LANGUAGE FunctionalDependencies #-}

module DMCTS.Types where

import GHC.Generics
import Data.Aeson
import Data.Tree
import System.Random

import Network.HTTP.Client

-- Defines required methods for nodes in our Monte Carlo Search Tree.
-- NOTE: I've set up the functional dependencies to pin label and weight to a
--       specific set of logic. There are opposing forces within the software
--       design that led to this decision. I want to be able to write handlers
--       without requiring a dummy label to make the types solvable. On the other
--       hand, being able to specify different weighting functions through combinations
--       of logic/label types (i.e: logic label -> weight) is pretty enticing.
--       We'll see if I eventually find a way to work through it.
--
-- NOTE: Since aggregation is no longer hardcoded in the runtime, I've removed
--       the Num restriction on weight. For aggregation functions where we're
--       looking to find a min/max weight, we could package the label as a part
--       of the weight.
class (ToJSON label, FromJSON label,
       ToJSON weight, FromJSON weight) =>
      WeightAble logic label weight
        | logic -> label weight where
  children  :: logic -> label -> [label]
  weight    :: logic -> label -> weight
  aggregate :: logic -> [weight] -> weight

-- TODO: Either impose additional type constraints, or eliminate this.
type WeightTree a = Tree a

-- Our DMCTS Runtime supports a number of different sampling methods
data SampMethodE =
  SampLeaf  |
  SampSpine deriving (Enum, Show, Generic, Eq)

instance FromJSON SampMethodE
instance ToJSON SampMethodE

-- The datatypes used to communicate with instances of the DMCTS runtime.
data DMCTSRequest label = DMCTSRequest { method   :: SampMethodE
                                       , nSamp    :: Int
                                       , root     :: label
                                       , randSeed :: Int
                                       } deriving (Show, Generic)

newtype DMCTSResponse weight = DMCTSResponse { result :: weight
                                             } deriving (Show, Generic)

-- The configuration for the DMCTS client. As is, only holds the URL of the API
-- end point. Could hold authentication information in the future.
data DMCTSLambdaConfig = DMCTSLambdaConfig { url            :: String
                                           , sampsPerLambda :: Int
                                           } deriving (Show, Generic)

data DMCTSClientState = DMCTSClientState { lambdaConfig :: DMCTSLambdaConfig
                                         , httpManager  :: Manager
                                         }

instance FromJSON DMCTSLambdaConfig
instance ToJSON DMCTSLambdaConfig
