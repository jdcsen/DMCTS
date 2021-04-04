# Milestones

## Infrastructure/Deployment
1. Deploy a dummy instance of the runtime to AWS Lambda in order to verify we can run Haskell code.
	1. Verified through communicating with the runtime through the AWS CLI.
2. Communicate with an AWS Lambda instance in Haskell code.
	1. Verified by communicating with the dummy instance deployed above

## Major Components
1. dmcts-runtime
	1. Application-specific IO
		1. Threshold: Command and read Monte Carlo Tree semantics from AS Software.
	2. Runtime I/O (Might be provided by our base runtime)
		1. Threshold: Pass a root node to the runtime, get an average node value back.
		2. Stretch: Multidimensional node values
		3. Stretch: Use with AWS API Gateway
	3. Monte-Carlo Tree Sampling
		1. Threshold: Single-instance sampling
		2. Stretch: Recursive sampling (runtime triggers more runtimes)
2. dmcts-encoding
	1. dmcts-runtime Communication Protocol (Aeson JSON Objects)
		1. Threshold: MVP communication protocol that allows all threshold-level tasks to be communicated to the runtime.
	2. User-friendly functions to trigger compute on remote dmcts-runtime instances without making manual AWS calls.
		1. Threshold: Hard coded credentials/settings
		2. Stretch: User specified configuration file
	3. Wrappers for Application-specific haskell software to report Monte Carlo Tree semantics through pure function calls
		1. Threshold: A wrapper to allow users to define pure Haskell functions that report Monte Carlo Tree semantics without dealing with I/O
3. dmcts-knapsack
	1. Application-specific software to report state of the Monte Carlo Tree
		1. Threshold: Using the dmcts-encoding helper functions, report Knapsack Problem semantics to the dmcts-runtime
	2. AWS Lambda instance for dmcts-knapsack
		1. Threshold: Deploy an AWS Lambda function that combines the dmcts-runtime with the knapsack application software
	3. Client code to drive dmcts-knapsack runtime
		1. Threshold: Hard coded Knapsack Problem parameters, with optimal solution determined by the dmcts-knapsack AWS Lambda instance.
