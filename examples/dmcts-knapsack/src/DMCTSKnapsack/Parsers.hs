{-# LANGUAGE GADTs #-}
module DMCTSKnapsack.Parsers where

import Data.Aeson

import Options.Applicative
import Data.Semigroup ((<>))

import DMCTSKnapsack.KnapTree

import Criterion.Main.Options

-- Knapsack Problem argument parsing.
data KnapArgs where
  SingleRun  :: { local :: Bool,
                  knapCfgPath :: String,
                  lambdaCfgPath :: String,
                  mcSamples :: Int} -> KnapArgs
  Benchmark  :: { knapCfgPath :: String,
                  lambdaCfgPath :: String,
                  minSamp :: Int,
                  sampBase :: Int,
                  nBenchmarks :: Int,
                  nRuns :: Int} -> KnapArgs
  deriving (Show)

cmdParser :: Parser KnapArgs
cmdParser = hsubparser
  (command "single"
      (info singleParser
        ( progDesc "Sample a single Knapsack problem." )) <>
   command "bench"
      (info benchParser
        ( progDesc "Run a suite of benchmarks comparing local and remote sampling." ))
  )

singleParser :: Parser KnapArgs
singleParser = SingleRun
      <$> switch
          ( long "local"
         <> short 'l'
         <> help "If set, runs the calculations locally." )
      <*> strOption
          ( long "knapsack-cfg-path"
         <> short 'k'
         <> metavar "KNAPSACK-CFG-PATH"
         <> showDefault
         <> value "./knapsack-cfg.json"
         <> help "The configuration path for the knapsack problem to be solved." )
      <*> strOption
          ( long "lambda-cfg-path"
         <> short 'c'
         <> metavar "LAMBDA-CFG-PATH"
         <> showDefault
         <> value "./lambda-cfg.json"
         <> help "The configuration path for our DMCTS Lambda Config" )
      <*> option auto
          ( long "monte-carlo-samples"
         <> short 's'
         <> metavar "MONTE-CARLO-SAMPLES"
         <> showDefault
         <> value 100
         <> help "The number of samples to run per Monte Carlo run" )

benchParser :: Parser KnapArgs
benchParser = Benchmark
      <$> strOption
          ( long "knapsack-cfg-path"
         <> short 'k'
         <> metavar "KNAPSACK-CFG-PATH"
         <> showDefault
         <> value "./knapsack-cfg.json"
         <> help "The configuration path for the knapsack problem to be solved." )
      <*> strOption
          ( long "lambda-cfg-path"
         <> short 'c'
         <> metavar "LAMBDA-CFG-PATH"
         <> showDefault
         <> value "./lambda-cfg.json"
         <> help "The configuration path for our DMCTS Lambda Config" )
      <*> option auto
          ( long "min-sample"
         <> short 'n'
         <> metavar "MIN-SAMPLE"
         <> showDefault
         <> value 1
         <> help "The minimum number of samples to benchmark" )
      <*> option auto
          ( long "exp-sample-base"
         <> short 'x'
         <> metavar "EXP-SAMPLE-BASE"
         <> showDefault
         <> value 10
         <> help "The base for the exponential sampling" )
      <*> option auto
          ( long "num-benchmarks"
         <> short 'b'
         <> metavar "NUM-BENCHMARKS"
         <> showDefault
         <> value 7
         <> help "The number of benchmarks to execute. Benchmark samples are calculated by MIN-SAMPLE+EXP_SAMPLE_BASE^(sample number)")
      <*> option auto
          ( long "num-runs"
         <> short 'r'
         <> metavar "NUM-RUNS"
         <> showDefault
         <> value 1
         <> help "The number of runs to execute per benchmark." )


newKnapCfg :: FilePath -> IO (Maybe KnapNode)
newKnapCfg = decodeFileStrict
