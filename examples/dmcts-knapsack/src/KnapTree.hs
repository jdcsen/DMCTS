{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module KnapTree where

import GHC.Generics
import Data.Aeson
import Data.List

import DMCTS.Types

data KnapNode = KnapNode [Integer] Integer deriving (Show, Generic)

-- Ensure our label data structure can be ser/deser
instance FromJSON KnapNode
instance ToJSON KnapNode

-- Create instances of fromJSON and toJSON for our payloads
instance FromJSON (DMCTSRequest KnapNode)
instance ToJSON (DMCTSRequest KnapNode)

instance FromJSON (DMCTSResponse KnapNode Integer)
instance ToJSON (DMCTSResponse KnapNode Integer)

-- Adds an item to the knapsack, without replacement.
pickItemNR :: KnapNode -> Int -> KnapNode
pickItemNR input@(KnapNode options remaining) idx = KnapNode newOptions newRemaining
  where
    pick = options !! idx
    newRemaining = pick + remaining
    (head, tail) = splitAt idx options
    newOptions = head ++ drop 1 tail

-- Adds an item to the knapsack, with replacement.
pickItemWR :: KnapNode -> Int -> KnapNode
pickItemWR input@(KnapNode options remaining) idx = KnapNode options newRemaining
  where
    pick = options !! idx
    newRemaining = pick + remaining


-- Two sets of logic: One for operations with replacement, one for ones without.
data KnapLogicWR = KnapLogicWR
instance WeightAble KnapLogicNR KnapNode Integer where
  weight    _ (KnapNode _ w) = w
  children  _ node@(KnapNode o w) = scanl pickItemNR node [0..(length o - 1)]
  aggregate _ weights = floor (toRational (sum weights) / toRational (length weights))

data KnapLogicNR = KnapLogicNR
instance WeightAble KnapLogicWR KnapNode Integer where
  weight    _ (KnapNode _ w) = w
  children  _ node@(KnapNode o w) = scanl pickItemWR node [0..(length o - 1)]
  aggregate _ weights = floor (toRational (sum weights) / toRational (length weights))

