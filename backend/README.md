backend
=====

An OTP application

Distributed Hash Table with docker
----------------------------------

Create a network

    $ docker network create --subnet 192.168.165.1/24 cdc21-project

You can either compile the image and start a container with this compiled image:

    $ docker build -t cdc21project-backend .
    $ docker run --rm -it --name backend -h backend --network cdc21-project -p 8080:8080 --ip 192.168.165.2 cdc21project-backend

OR use the image on dockerhub

    $ docker run --rm -it --name backend -h backend --network cdc21-project kevinberret/cdc21project-backend:latest

Launch as many other nodes as needed (change the hostname for each node)

    $ docker run --rm -it --hostname node3 --network cdc21-project -v $(pwd)/backend:/data erlang /data/run_node.sh

In each node, in the erl shell, run the next commands:

    $ Id = key:generate().
    $ NewNode = node:start(Id, {node0, 'node0@192.168.165.2'}).


