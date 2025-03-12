
FROM elixir:1.14-otp-26 AS builder

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends     postgresql-client

# Install hex and rebar
RUN mix local.hex --force
RUN mix local.rebar --force

# Copy project files
COPY mix.exs .
COPY mix.lock .

# Get deps
RUN mix deps.get
RUN mix deps.compile

COPY config config
COPY lib lib
COPY priv priv

# Compile the project
RUN mix compile

# Release
RUN mix release

FROM alpine:3.18 AS release

WORKDIR /app

RUN apk update && apk add --no-cache libstdc++ postgresql-client

# Copy the release from the builder stage
COPY --from=builder /app/_build/prod/rel/complang ./

# Set the port
EXPOSE 8080

# Run the application
CMD ["/app/bin/complang", "start"]
