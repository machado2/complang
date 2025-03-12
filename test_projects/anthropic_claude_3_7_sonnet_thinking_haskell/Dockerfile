FROM haskell:9.0 as build

WORKDIR /app

# Copy cabal file
COPY haskell-crud-api.cabal .

# Install dependencies
RUN cabal update && cabal build --only-dependencies -j4

# Copy source files
COPY src/ src/

# Build the application
RUN cabal install --installdir=/app/bin --overwrite-policy=always

# Use a smaller base image for the final image
FROM debian:bullseye-slim

# Install required runtime dependencies
RUN apt-get update &&     apt-get install -y libpq-dev ca-certificates &&     apt-get clean &&     rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the built executable
COPY --from=build /app/bin/haskell-crud-api /app/

# Expose the port
EXPOSE 8080

# Set environment variable
ENV PGPASSWORD=""

# Run the application
CMD ["/app/haskell-crud-api"]
