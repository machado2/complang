
FROM ubuntu:22.04

# Set non-interactive installation mode
ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    libboost-all-dev \
    libpq-dev \
    pkg-config \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /app

# Copy source files
COPY main.cpp .
COPY CMakeLists.txt .

# Build the application
RUN mkdir -p build && cd build && cmake .. && make -j$(nproc)

# Expose port 8080
EXPOSE 8080

# Set environment variable (will be overridden at runtime)
ENV PGPASSWORD=""

# Run the application
CMD ["/app/build/crud_api"]
