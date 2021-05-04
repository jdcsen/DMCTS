{-# LANGUAGE FlexibleContexts #-}

module DMCTS.Client.Sampling where

import Data.Aeson

import DMCTS.Types
import DMCTS.Requests

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import System.Random

-- Local Sampling methods
-- Given a DMCTSClientState and a root node, takes samples of random root-leaf
-- spines locally.
sampSpinesL :: (WeightAble l a w) => l -> a -> Int -> StdGen -> (w, StdGen)
sampSpinesL = execDMCTSLocal SampSpine

-- Given a DMCTSClientState and a root node, takes samples of random leaf nodes
-- descended from the root locally.
sampLeafsL :: (WeightAble l a w) => l -> a -> Int -> StdGen -> (w, StdGen)
sampLeafsL = execDMCTSLocal SampLeaf

-- Remote Sampling methods

-- Given a DMCTSClientState and a root node, takes samples of random root-leaf
-- spines by offloading the calculations to the configured lambda instances.
sampSpinesR ::
  (WeightAble l a w,
   ToJSON (DMCTSRequest a),
   FromJSON (DMCTSResponse w)) =>
    DMCTSClientState ->
    l ->
    a ->
    Int ->
    StdGen ->
    IO (w, StdGen)
sampSpinesR = execDMCTSRemote SampSpine

-- Given a DMCTSClientState and a root node, takes samples of random leaf nodes
-- descended from the root by offloading the calculations to the configured
-- lambda instances.
sampLeafsR ::
  (WeightAble l a w,
  ToJSON (DMCTSRequest a),
  FromJSON (DMCTSResponse w)) =>
    DMCTSClientState ->
    l ->
    a ->
    Int ->
    StdGen ->
    IO (w, StdGen)
sampLeafsR = execDMCTSRemote SampLeaf
