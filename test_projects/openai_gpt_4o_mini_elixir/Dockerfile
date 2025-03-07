
# Use the official Elixir image
FROM elixir:1.12

# Set the working directory
WORKDIR /app

# Install Hex and Rebar
RUN mix local.hex --force && mix local.rebar --force

# Install necessary dependencies
COPY mix.exs mix.lock ./
RUN mix deps.get

# Copy the application files
COPY lib ./lib
COPY config ./config

# Compile the application
RUN mix compile

# Expose the port for the application
EXPOSE 8080

# Start the Phoenix server
CMD ["mix", "phx.server"]
