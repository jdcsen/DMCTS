# DMCTS (Distributed Monte Carlo Tree Search)
This software system provides libraries and Docker base images to implement a Monte Carlo Tree Search that can be distributed and run on AWS Lambda instances. In order to be compatible with a wide number of use cases, each node in the Monte Carlo tree is represented by a user-defined Unicode strings, with branching semantics and node value information provided by a user-defined software component derived from the dmcts-runtime base image.

## dmcts-encoding
The DMCTS Encoding library provides an interface for communicating with dmcts-runtime instances. For library details, see the dmcts-encoding README.

## dmcts-runtime
The DMCTS Runtime provides a Docker base image that is designed to be extended with application-specific software that generates the children for any node in the Monte Carlo tree. For information on the interface that the application software must provide, see the dmcts-runtime README.
