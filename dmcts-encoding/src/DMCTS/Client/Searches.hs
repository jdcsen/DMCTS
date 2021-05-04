{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMCTS.Client.Searches where

import Data.List
import Data.Aeson

import System.Random

import DMCTS.Types
import DMCTS.Random
import DMCTS.Client.Sampling

import Control.Monad.Zip

-- Given a root node, explores the tree from that root, finding the DMCTS minimum
-- node by way of the specified min function. At each node, takes the specified number
-- of samples to determine the next step.
minLeafL ::
  (WeightAble l a w,
   ToJSON (DMCTSRequest a),
   FromJSON (DMCTSResponse w)) =>
    l ->
    a ->
    Int ->
    (w->w->Ordering) ->
    StdGen ->
    (a, StdGen)
minLeafL logic node nSamp orderF gen
  | null nodeChildren = (node, gen) -- Base case: no children
  | otherwise = minLeafL logic minChild nSamp orderF newGen-- Recursive case: search again
  where
    -- Generate children
    nodeChildren = children logic node
    -- Generate random seeds for each child.
    (newGen, seeds) = mapAccumL (\g _ -> split g) gen nodeChildren
    -- Collect our samples.
    (samps, _) = unzip $ zipWith4 sampLeafsL (repeat logic) nodeChildren (repeat nSamp) seeds
    sampedChildren = zip samps nodeChildren
    -- Find the minimum child
    (_, minChild) = minimumBy (\(sampR, _) (sampL, _) -> orderF sampR sampL) sampedChildren

-- Given a root node, explores the tree from that root, finding the DMCTS minimum
-- node by way of the specified min function. At each node, takes the specified number
-- of samples to determine the next step.
minLeafR ::
  forall l a w.
    (WeightAble l a w,
     ToJSON (DMCTSRequest a),
     FromJSON (DMCTSResponse w)) =>
      DMCTSClientState ->
      l ->
      a ->
      Int ->
      (w->w->Ordering) ->
      StdGen ->
      IO (a, StdGen)
minLeafR cs logic node nSamp orderF gen
  | null nodeChildren = return (node, gen) -- Base case: no children
  | otherwise = do
    -- Find min and recurse.
    sampled <- sequence sampSeq :: IO [(w, StdGen)]
    let (samps, _) = unzip sampled
        sampedChildren = zip samps nodeChildren
        -- Find the minimum child
        (_, minChild) = minimumBy (\(sampR, _) (sampL, _) -> orderF sampR sampL) sampedChildren

    minLeafR cs logic minChild nSamp orderF newGen :: IO (a, StdGen)-- Recursive case: search again
  where
    -- Generate children
    nodeChildren = children logic node
    -- Generate random seeds for each child.
    (seeds, newGen) = getNGens gen $ length nodeChildren
    -- Build a set of samples to be collected.
    sampSeq =
      zipWith5
        sampLeafsR
        (repeat cs)
        (repeat logic)
        nodeChildren
        (repeat nSamp)
        seeds :: [IO (w, StdGen)]



