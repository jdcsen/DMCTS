module DMCTS.SearchTree where

import GHC.Generics
import Data.Aeson
import Data.Tree
import System.Random

import DMCTS.Types

-- Builds a weighted tree from a root node.
mkWeightTree :: (WeightAble l a w) => l -> a -> WeightTree a
mkWeightTree logic = unfoldTree unfolder
  where
    unfolder = \node -> (node, children logic node)

-- Helper function to help extract a random child from a tree.
randChild :: Tree a -> StdGen -> (Tree a, StdGen)
randChild (Node _ children) gen = (child, newGen)
  where
    (idx, newGen) = randomR (0, length children - 1) gen
    child = children !! idx

-- Samples the weight of a random leaf node in a weighted tree
sampLeaf ::  (WeightAble l a w) => l -> WeightTree a -> StdGen -> (w, StdGen)

-- Base case: Leaf node
sampLeaf logic (Node root []) gen = (weight logic root, gen)

-- Recursive case: a random child
sampLeaf logic tree@(Node root children) gen = sampLeaf logic child newGen
  where
    (child, newGen) = randChild tree gen


-- Samples a random spine, from root to leaf, of a weighted tree.
sampSpine ::  (WeightAble l a w) => l -> WeightTree a -> StdGen -> ([w], StdGen)

-- Base case: Leaf node
sampSpine logic (Node root []) gen = ([weight logic root], gen)

-- Recursive case:
-- TODO: If this form of recursion ends up being a performance issue, optimize.
sampSpine logic tree@(Node root children) gen = (weight logic root : rList, rGen)
  where
    (child, newGen) = randChild tree gen
    (rList, rGen) = sampSpine logic child newGen

aggSpine ::  (WeightAble l a w) => l -> WeightTree a -> StdGen -> (w, StdGen)
aggSpine logic tree gen = (agg, newGen)
  where
    (spine, newGen) = sampSpine logic tree gen
    agg = aggregate logic spine

-- Given a DMCTSRequest, executes the request and returns the DMCTSResponse
execRequest :: (WeightAble l a w) => l -> DMCTSRequest a -> DMCTSResponse a w

-- Leaf handler
execRequest logic DMCTSRequest {method=AvgLeaf, ..} = response
  where
    gen = mkStdGen randSeed
    response = DMCTSResponse {result = weight logic root}

-- Spine handler
execRequest logic DMCTSRequest {method=AvgSpine, ..} = response
  where
    gen = mkStdGen randSeed
    response = DMCTSResponse {result = weight logic root}

