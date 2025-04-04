.PHONY: default build start clean nuke

image := mbt-server-image
container := mbt-server-container

default: build

build: 
	docker build --tag $(image) .
	-docker rm $(container)
	docker create -it --name $(container) -p 8080:80 $(image)

start:
	docker start -i $(container) 

# Docker does not offer a way to ignore non-existing images or containers, so ignore result
clean:
	-docker rm $(container)
	-docker rmi $(image)

# Remove everything
nuke:
	docker system prune -a