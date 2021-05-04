# Default environment variables.
dummy : export DOCKER_IMAGE_NAME = dmcts-dummy-runtime
dummy : export EXECUTABLE_NAME = dmcts-dummy-lambda

knapsack : export DOCKER_IMAGE_NAME = dmcts-knapsack-runtime
knapsack : export EXECUTABLE_NAME = dmcts-knapsack-runtime

knapsack:
	DOCKER_BUILDKIT=1 docker build --build-arg EXECUTABLE_NAME=${EXECUTABLE_NAME} --file Lambda.Dockerfile -t ${DOCKER_IMAGE_NAME}:latest-dev .
	docker tag  ${DOCKER_IMAGE_NAME}:latest-dev 544864025547.dkr.ecr.us-west-1.amazonaws.com/${DOCKER_IMAGE_NAME}:latest-dev
	docker push 544864025547.dkr.ecr.us-west-1.amazonaws.com/${DOCKER_IMAGE_NAME}:latest-dev

dummy:
	DOCKER_BUILDKIT=1 docker build --build-arg EXECUTABLE_NAME=${EXECUTABLE_NAME} --file Lambda.Dockerfile -t ${DOCKER_IMAGE_NAME}:latest-dev .
	docker tag  ${DOCKER_IMAGE_NAME}:latest-dev 544864025547.dkr.ecr.us-west-1.amazonaws.com/${DOCKER_IMAGE_NAME}:latest-dev
	docker push 544864025547.dkr.ecr.us-west-1.amazonaws.com/${DOCKER_IMAGE_NAME}:latest-dev

