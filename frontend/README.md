# Frontend

This is a frontend allowing to interact with the Chordy implementation done in the "backend" folder.

## Development server

Run `ng serve` for a dev server. Navigate to `http://localhost:4200/`. The app will automatically reload if you change any of the source files.

## Dockerisation

Build the frontend image and start the container

    $ docker build -t cdc21project-frontend . 
    $ docker run -d -p 80:80 cdc21project-frontend

OR use the image on dockerhub

    $ docker run -d -p 80:80 kevinberret/cdc21project-frontend:latest

Open a web browser and go to the following url

    $ http://localhost
