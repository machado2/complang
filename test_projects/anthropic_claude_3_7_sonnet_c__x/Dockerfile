FROM ubuntu:22.04

# Update packages and install dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    curl \
    libpq-dev \
    pkg-config \
    wget \
    unzip \
    libpqxx-dev \
    libboost-all-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Crow
RUN git clone https://github.com/CrowCpp/Crow.git \
    && cd Crow \
    && mkdir build \
    && cd build \
    && cmake .. \
    && cmake --build . \
    && cmake --install . \
    && cd ../.. \
    && rm -rf Crow

# Create app directory
WORKDIR /app

# Copy source files
COPY . .

# Build the application
RUN cmake -B build && cmake --build build

# Expose the port
EXPOSE 8080

# Run the application
CMD ["./build/crud_api"]
