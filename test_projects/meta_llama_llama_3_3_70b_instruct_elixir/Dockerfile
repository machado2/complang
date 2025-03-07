
FROM elixir:1.13

# Set working directory to /app
WORKDIR /app

# Copy mix.exs
COPY mix.exs.

# Install dependencies
RUN mix deps.get

# Copy mix.lock and application code
COPY..

# Compile the application
RUN mix compile

# Expose port 8080
EXPOSE 8080

# Run the application
CMD ["mix", "run", "--no-halt"]
