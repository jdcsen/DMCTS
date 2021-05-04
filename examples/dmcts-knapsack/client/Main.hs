module Main where

import System.Environment
import System.Random

import Data.Maybe

import Options.Applicative

import Control.Monad.Trans.Maybe

import DMCTS.Client.State
import DMCTS.Client.Sampling
import DMCTS.Client.Searches
import DMCTS.Types
import DMCTS.Random

import DMCTSKnapsack.KnapTree
import DMCTSKnapsack.Parsers

import Criterion.Main
import Criterion.Main.Options
import qualified Criterion.Types as CT

import Statistics.Types

main :: IO ()
main = validator =<< execParser opts
  where
    opts = info (cmdParser <**> helper)
      ( fullDesc
     <> progDesc "Estimate solutions for knapsack problems"
     <> header "dmcts-knapsack-client - a client for estimating Knapsack Problems with DMCTS" )

-- Validates all input arguments before calling execMtx
validator :: KnapArgs -> IO ()
validator args@SingleRun {..} = do
  knapNode' <- newKnapCfg knapCfgPath
  clientState' <- runMaybeT $ newClientState lambdaCfgPath
  -- Same dirty error checking as above.
  -- TODO: Tweak this to print a friendly error message.
  let clientState = fromJust clientState'
  let knapNode = fromJust knapNode'
  execMtx args clientState knapNode

validator args@Benchmark {..} = do
  knapNode' <- newKnapCfg knapCfgPath
  clientState' <- runMaybeT $ newClientState lambdaCfgPath
  -- Same dirty error checking as above.
  -- TODO: Tweak this to print a friendly error message.
  let clientState = fromJust clientState'
  let knapNode = fromJust knapNode'
  execMtx args clientState knapNode


-- Given a set of arguments, calls the appropriate functions to evaluate them
-- and print the results.
--
-- NOTE: This _might_ be an anti-pattern. I'll have to see how it pans out, I haven't
--       figured out the proper design idioms for this kind of functional code yet.
--       I do like being able to implement switching logic through pattern matching
--       though.
execMtx :: KnapArgs -> DMCTSClientState -> KnapNode -> IO ()

-- Single run cases.
execMtx SingleRun {local=True, ..} cs node = do
  gen <- getStdGen
  putStrLn "Running a search locally..."
  (minNode, _) <- return $ minLeafL KnapLogicWR node mcSamples compare gen
  putStrLn $ "Solution: " ++ show minNode

-- Cases where computation should be offloaded to AWS lambda instances
execMtx SingleRun {local=False, ..} cs node = do
  -- Run the calculation.
  gen <- getStdGen
  putStrLn "Running a search remotely..."
  (minNode, _) <- minLeafR cs KnapLogicWR node mcSamples compare gen
  putStrLn $ "Solution: " ++ show minNode

-- Benchmark case.
execMtx args@Benchmark {..} cs node = do
  putStrLn "Running a benchmark..."
  gen <- getStdGen
  let runBench = buildBench args cs node gen
  runBench


buildBench :: KnapArgs -> DMCTSClientState -> KnapNode -> StdGen -> IO ()
buildBench Benchmark {..} cs node gen = runBench
  where
    -- Lambdas to assist in building the benchmarks
    localSamp  = \sampCnt gen -> fst $ minLeafL KnapLogicWR node sampCnt compare gen
    remoteSamp = \sampCnt gen -> fst <$> minLeafR cs KnapLogicWR node sampCnt compare gen
    -- Build the list of sample #'s for the benchmarks.
    exponents = [0..(nBenchmarks-1)] :: [Int]
    benchSamples = filter (/= 0) $ map ((minSamp+).(sampBase^)) exponents
    benchNames = map (\a -> "benchmark_samp_" ++ show a) benchSamples

    localFuncs  = map localSamp benchSamples
    remoteFuncs = map remoteSamp benchSamples

    localBenches = zipWith whnf localFuncs lSeeds :: [Benchmarkable]
    remoteBenches = zipWith whnfAppIO remoteFuncs lSeeds :: [Benchmarkable]

    -- Generate seeds
    (lSeeds, newGen) = getNGens gen (length benchSamples)
    (rSeeds, _)      = getNGens newGen (length benchSamples)

    localBenchmarks = bgroup "Local" $ zipWith bench benchNames localBenches
    remoteBenchmarks = bgroup "Remote" $ zipWith bench benchNames remoteBenches

    -- Replace benchmark configuration
    criterionArgs =
      defaultConfig
        { --CT.confInterval = cl90,
          --CT.resamples = nRuns,
          --CT.timeLimit = 600,
          CT.reportFile = Just "./bench-report.html",
          CT.csvFile    = Just "./bench-data.csv",
          CT.verbosity  = CT.Verbose}
    critMode = Run criterionArgs IPattern []
    --args = criterionArgs { CT.resamples = nRuns}
    runBench = runMode critMode [localBenchmarks, remoteBenchmarks]
