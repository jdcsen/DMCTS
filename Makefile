all:
	DOCKER_BUILDKIT=1 docker build --file Lambda.Dockerfile -t dmcts-runtime-lambda .
	docker tag  dmcts-runtime-lambda:latest 544864025547.dkr.ecr.us-west-1.amazonaws.com/dmcts-runtime-lambda:latest
	docker push 544864025547.dkr.ecr.us-west-1.amazonaws.com/dmcts-runtime-lambda:latest
