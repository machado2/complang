
FROM elixir:1.14-alpine AS build

# Install build dependencies
RUN apk add --no-cache build-base git

# Create app directory and copy the Elixir project into it
WORKDIR /app
COPY . .

# Install Hex package manager and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Fetch dependencies and compile
RUN mix deps.get
RUN MIX_ENV=prod mix compile

# Create release
RUN mix release

# Start a new build stage
FROM alpine:3.17

RUN apk add --no-cache libstdc++ ncurses-libs openssl

WORKDIR /app

# Copy the release from the build stage
COPY --from=build /app/_build/prod/rel/users_api ./

# Set an environment variable for the PGPASSWORD
ENV PGPASSWORD="Saloon5-Moody-Observing"

# Expose port 8080
EXPOSE 8080

# Create a script to run migrations and start the application
RUN echo '#!/bin/sh\n\
./bin/users_api eval "UsersApi.Release.migrate()"\n\
./bin/users_api start\n\
' > /app/entrypoint.sh

# Make the entrypoint script executable
RUN chmod +x /app/entrypoint.sh

# Start the application using the entrypoint script
CMD ["/app/entrypoint.sh"]
