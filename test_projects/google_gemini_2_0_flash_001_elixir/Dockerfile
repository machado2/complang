
FROM elixir:1.15-otp-26 AS builder

WORKDIR /app

# Install build dependencies
RUN apt-get update -y
RUN apt-get install -y build-essential postgresql-client libpq-dev git

# Install Hex and Rebar
RUN mix local.hex --force
RUN mix local.rebar --force

# Create Phoenix project
RUN mix phx.new --no-ecto --no-assets . --force --app complang

# Copy project source
COPY lib lib
COPY config config
COPY priv priv

#Copy router
COPY lib/complang_web/router.ex lib/complang_web/router.ex
COPY lib/complang_web/controllers/user_controller.ex lib/complang_web/controllers/user_controller.ex
COPY config/dev.exs config/dev.exs
#ENV
ENV SECRET_KEY_BASE="test"
ENV CASSANDRA_HOST=host.docker.internal
ENV CASSANDRA_PORT=9042
ENV PGPASSWORD=Saloon5-Moody-Observing

# Get dependencies
RUN mix deps.get


# Compile the application
RUN mix assets.deploy
RUN mix compile

# Build release
RUN mix release

# Release stage
FROM debian:stable-slim

WORKDIR /app

# Install runtime dependencies
RUN apt-get update -y
RUN apt-get install -y libstdc++ openssl postgresql-client

# Copy release from builder
COPY --from=builder /app/_build/prod/rel/complang ./

# Set environment variables
ENV PORT=8080
ENV SECRET_KEY_BASE="test"
ENV CASSANDRA_HOST=host.docker.internal
ENV CASSANDRA_PORT=9042
ENV PGPASSWORD=Saloon5-Moody-Observing

# Set port through the command line for vmargs
CMD ["bin/complang", "start"]

EXPOSE 8080
