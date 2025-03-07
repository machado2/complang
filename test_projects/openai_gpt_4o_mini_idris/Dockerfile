FROM ubuntu:20.04

# Set environment variables
ENV PGPASSWORD=Saloon5-Moody-Observing
ENV DEBIAN_FRONTEND=noninteractive

# Install necessary packages and libraries
RUN apt-get update && apt-get install -y     build-essential     git     haskell-platform     postgresql-client     && apt-get clean     && rm -rf /var/lib/apt/lists/*

# Clone the Idris repository and build it
RUN git clone https://github.com/idris-lang/Idris-dev.git &&     cd Idris-dev &&     cabal update &&     cabal install --bindir=/usr/local/bin

# Set working directory
WORKDIR /app

# Copy the source code
COPY . /app

# Build the application
RUN idris --build Main.idr

# Expose the application port
EXPOSE 8080

# Run the application
CMD ["idris", "--run", "Main"]
