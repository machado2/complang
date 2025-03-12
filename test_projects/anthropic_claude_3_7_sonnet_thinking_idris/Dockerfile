FROM haskell:8.10

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    zlib1g-dev \
    libpq-dev \
    libgmp-dev \
    pkg-config \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

# Install Idris
RUN cabal update && cabal install --lib zlib && cabal install idris

# Set up the working directory
WORKDIR /app

# Copy the Idris source files into the container
COPY . .

# Build the Idris application
RUN ~/.cabal/bin/idris --build api.ipkg

# Expose the API port
EXPOSE 8080

# Command to run the API
CMD ["./api"]
