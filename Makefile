all:
	@rm -rf ./build/*
	DOCKER_BUILDKIT=1 docker build --file Lambda.Dockerfile -t dmcts-runtime-lambda .
	id=$$(docker create dmcts-runtime-lambda); docker cp $$id:/root/output ./build; docker rm -v $$id
	cd build/output; zip -r function.zip *
