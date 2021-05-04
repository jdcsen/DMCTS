-- DMCTS-specific random number generation.
module DMCTS.Random where

import System.Random
import Data.List

-- Given a random number generator, splits that generator N times
--
-- NOTE: I'm suspicious of the practical randomness of this. I wonder about the
--       correlation between a sequence of split StdGen instances.
getNGens :: StdGen -> Int -> ([StdGen], StdGen)
getNGens gen nGen = (seeds, newGen)
  where
    (newGen, seeds) = mapAccumL (\g _ -> split g) gen (replicate nGen ())
