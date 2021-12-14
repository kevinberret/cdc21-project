# cdc21-project
This is a frontend allowing to interact with the Chordy implementation done in the "backend" folder.

## Dockerisation

Build frontend image

    $ docker build -t cdc21project-frontend . 

Start the frontend

    $ docker run -d -p 80:80 cdc21project-frontend

Open a web browser and go to the following url

    $ http://localhost
