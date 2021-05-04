{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Dummies where

import GHC.Generics

import Data.Aeson

import DMCTS.Types

-- Common dummy node
newtype DummyNode = DummyNode
  { dWeight :: Int } deriving (Show, Generic)

instance FromJSON DummyNode
instance ToJSON DummyNode

instance FromJSON (DMCTSRequest DummyNode)
instance ToJSON (DMCTSRequest DummyNode)

-- Fuzzy Stick: a tree of infinite depth. 
data FuzzyStick = FuzzyStick
instance WeightAble FuzzyStick DummyNode Int where
