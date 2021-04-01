# dmcts-runtime

The DMCTS Runtime provides functionality to evaluate and accumulate the MCTS from a specified root node.

## Application Software Interface
Currently, the dmcts-runtime retrieves branching and value information from an application specific software layer through a simple I/O stream interface. The interface is defined through simple key:value commands, all terminated with a newline. The software must:

* Start by running an executable located at '/DMCTS/app'
* Read key:value commands from stdin
* Print key:value responses to stdout
* Return an exit code of 0 when run successfully, and return an error code otherwise.

The commands the application software must support are:
### NOTE: Commands with required arguments are denoted with "<>", commands with optional arguments are denoted with "[]"

### Required Commands
* PARENT:<node-string>
	* Queried by the dmcts-runtime to determine the branching behavior of the Monte Carlo Tree at the specified node. The application software must respond with zero or more CHILD responses, terminated with a ENDCHILDLIST response.
* GETVALUE:<node-string>
	* Queried by the dmcts-runtime to determine the value of the specified node. The application software must respond with a single VALUE.

### Required Responses
* CHILD:<node-string>
	* Returned as a response to a PARENT command. Represents a single child of the node passed in PARENT. All child nodes are returned as individual CHILD responses.
* ENDCHILDLIST:<parent-node-string>
	* Returned when the application software is finished listing the children of the specified parent node.
* VALUE:[float]
	* Returned as a response to a GETVALUE command. Note: the float returned must be parsable by the Haskell "read" function. If a node's value should not be included in the final average returned to the caller, return an empty VALUE (i.e: "VALUE:\n")
