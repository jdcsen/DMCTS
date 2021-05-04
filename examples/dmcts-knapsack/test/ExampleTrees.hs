module ExampleTrees where

import DMCTSKnapsack.KnapTree

-- An example KnapNode with no opt
exNoOpt = KnapNode {opt=[], remSpace=10, choices=[]}

-- An example KnapNode with no space.
exNoSpace = KnapNode {opt=[1,2,3,4], remSpace=0, choices=[]}
