# Build stage 0
FROM erlang:alpine

RUN apk add --no-cache tar git bash make libc-dev gcc g++ vim

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang test application
COPY . backend

# And build the release
WORKDIR backend
RUN rebar3 as prod release

# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    # next line is very important!!!
    apk add --no-cache libstdc++  

# Install the released application
COPY --from=0 /buildroot/backend/_build/prod/rel/backend /release

# Expose relevant ports
EXPOSE 8080
EXPOSE 8443

CMD ["/release/bin/backend", "foreground"]
