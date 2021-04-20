{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module KnapTree where

import GHC.Generics
import Data.Aeson
import Data.List

import DMCTS.Types

data KnapNodeNR = KnapNodeNR [Integer] Integer deriving (Show, Generic)

instance FromJSON KnapNodeNR
instance ToJSON KnapNodeNR

data KnapNodeWR = KnapNodeWR [Integer] Integer deriving (Show, Generic)

instance FromJSON KnapNodeWR
instance ToJSON KnapNodeWR

-- Adds an item to the knapsack, without replacement.
pickItemNR :: KnapNodeNR -> Int -> KnapNodeNR
pickItemNR input@(KnapNodeNR options remaining) idx = KnapNodeNR newOptions newRemaining
  where
    pick = options !! idx
    newRemaining = pick + remaining
    (head, tail) = splitAt idx options
    newOptions = head ++ drop 1 tail

-- Adds an item to the knapsack, with replacement.
pickItemWR :: KnapNodeWR -> Int -> KnapNodeWR
pickItemWR input@(KnapNodeWR options remaining) idx = KnapNodeWR options newRemaining
  where
    pick = options !! idx
    newRemaining = pick + remaining

data KnapLogicNR = KnapLogicNR

data KnapLogicWR = KnapLogicWR

instance WeightAble KnapLogicNR KnapNodeNR Integer where
  weight _ (KnapNodeNR _ w) = w
  children _ node@(KnapNodeNR o w) = scanl pickItemNR node [0..(length o - 1)]

instance WeightAble KnapLogicWR KnapNodeWR Integer where
  weight _ (KnapNodeWR _ w) = w
  children _ node@(KnapNodeWR o w) = scanl pickItemWR node [0..(length o - 1)]

-- Create instances of fromJSON and toJSON for our payloads
instance FromJSON (DMCTSRequest KnapNodeNR)
instance ToJSON (DMCTSRequest KnapNodeNR)

instance FromJSON (DMCTSResponse KnapNodeNR Integer)
instance ToJSON (DMCTSResponse KnapNodeNR Integer)
