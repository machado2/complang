
FROM elixir:1.12-alpine

# Install build dependencies
RUN apk update && apk add --no-cache build-base npm git

WORKDIR /app

# Copy mix.exs and configuration; mix.lock is not copied as it will be generated during build
COPY mix.exs ./
COPY config config/
RUN mix local.hex --force && mix local.rebar --force
RUN mix deps.get

# Copy the rest of the application
COPY . .

# Compile the project in prod mode
RUN MIX_ENV=prod mix compile

EXPOSE 8080

CMD ["mix", "run", "--no-halt"]
