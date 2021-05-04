{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module DMCTSKnapsack.KnapTree where

import GHC.Generics
import Data.Aeson
import Data.List

import DMCTS.Types

data KnapNode = KnapNode
  { opt      :: [Integer]
  , remSpace :: Integer
  , choices  :: [Integer]
  } deriving (Show, Generic, Eq)

gNullNode = KnapNode {opt=[], remSpace= -1, choices=[]}

-- Ensure our label data structure can be ser/deser
instance FromJSON KnapNode
instance ToJSON KnapNode

-- Create instances of fromJSON and toJSON for our payloads
instance FromJSON (DMCTSRequest KnapNode)
instance ToJSON (DMCTSRequest KnapNode)

instance FromJSON (DMCTSResponse Rational)
instance ToJSON (DMCTSResponse Rational)

-- Adds an item to the knapsack, without replacement.
pickItemNR :: KnapNode -> Int -> KnapNode
pickItemNR input@KnapNode{opt=oldOptions, remSpace=oldRemaining, choices=oldChoices} idx = result
  where
    pick = oldOptions !! idx
    (head, tail) = splitAt idx oldOptions
    -- Remove our pick from options.
    opt = head ++ drop 1 tail
    -- Subtract our pick from our remainingSpace
    remSpace = oldRemaining - pick
    -- Record our choice
    choices = pick : oldChoices
    -- Build our result.
    result = KnapNode{..}

-- Adds an item to the knapsack, with replacement.
pickItemWR :: KnapNode -> Int -> KnapNode
pickItemWR input@KnapNode{remSpace=oldRemaining, choices=oldChoices, ..} idx = result
  where
    -- Just subtract our pick from remaining, leave options alone.
    pick = opt !! idx
    -- Subtract our pick from remainingSpace
    remSpace = oldRemaining - pick
    -- Record our choice
    choices = pick : oldChoices
    -- Build our result.
    result = KnapNode{..}

filtNodes :: [KnapNode] -> [KnapNode]
filtNodes = filter (\a -> 0 <= remSpace a)

-- Two sets of logic: One for operations with replacement, one for ones without.
data KnapLogicNR = KnapLogicNR
instance WeightAble KnapLogicNR KnapNode Rational where
  weight    _ KnapNode {..} = toRational remSpace
  children  _ node@KnapNode{..} = filtNodes allOpt
    where
      pickItem = pickItemNR node
      allOpt = fmap pickItem [0..(length opt - 1)]
  aggregate _ weights = toRational (sum weights) / toRational (length weights)

data KnapLogicWR = KnapLogicWR
instance WeightAble KnapLogicWR KnapNode Rational where
  weight    _ KnapNode {..} = toRational remSpace
  children  _ node@KnapNode{..} = filtNodes allOpt
    where
      pickItem = pickItemWR node
      allOpt = fmap pickItem [0..(length opt - 1)]
  aggregate _ weights = toRational (sum weights) / toRational (length weights)

