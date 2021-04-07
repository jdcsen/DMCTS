all:
	DOCKER_BUILDKIT=1 docker build --file Lambda.Dockerfile -t dmcts-runtime-lambda:latest-dev .
	docker tag  dmcts-runtime-lambda:latest-dev 544864025547.dkr.ecr.us-west-1.amazonaws.com/dmcts-runtime-lambda:latest-dev
	docker push 544864025547.dkr.ecr.us-west-1.amazonaws.com/dmcts-runtime-lambda:latest-dev
