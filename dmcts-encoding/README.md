# dmcts-encoding

The dmcts-encoding library is included by both the runtime and by any potential clients.  dmcts-encoding provides the bulk of the code required to run DMCTS, including data representations and functions to build DMCTS sampling requests, as well as code to execute those requests. 

## A rough map of the code is as follows

```
dmcts-encoding
├── dmcts-encoding.cabal
├── package.yaml
├── README.md < You are here!
├── Setup.hs
├── src
│   └── DMCTS
│       ├── Client < All client side code lives here, barring "Types" for the WeightAble definition. Users should really only care about _this_.
│       │   ├── Sampling.hs < Single node sampling: Sample random downtrees, or random descendent leafs. Has both local and remote versions.
│       │   ├── Searches.hs < Currently, greedy MC Sample based search: Sample, find min node, repeat. Has both local and remote versions.
│       │   └── State.hs    < Defines client state required to execute remote Lambda workloads, currently just a per-lambda sample # and an HTTP API URL.
│       ├── PersonEnc.hs  < Dummy code, deprecated.
│       ├── Random.hs     < Provides a function to generate N StdGen instances. Comes in handy when building a Monte Carlo library.
│       ├── Requests.hs   < Functions to build and execute requests, both pure DMCTSRequests, and remote DMCTSRequests wrapped in HTTP-client code.
│       ├── SearchTree.hs < Monte Carlo sampling logic. Fairly concise, given it's haskell.
│       └── Types.hs < DMCTS Datatypes. Sampling method Enum, WeightAble class, DMCTSRequest/Response definitions.
└── test < Largely untouched testing skeleton.
    ├── DMCTS
    │   ├── ClientSpec.hs
    │   ├── RequestsSpec.hs
    │   ├── SearchTreeSpec.hs
    │   └── TypesSpec.hs
    ├── Dummies.hs
    └── Spec.hs
```


# Testing
As it stands, the vast majority of the testing suite remains a skeleton with dummy tests. As the project is refined further, this is expected to change.
