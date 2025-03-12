
FROM ubuntu:22.04

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    libpq-dev \
    libssl-dev \
    git \
    xdg-utils \
    --no-install-recommends \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install D compiler
RUN curl -fsS https://dlang.org/install.sh | bash -s dmd \
    && ln -s /root/dlang/dmd-*/activate /root/activate-dmd

WORKDIR /app

# Copy DUB configuration files
COPY dub.json ./
COPY source/ ./source/

# Build the application
SHELL ["/bin/bash", "-c"]
RUN source /root/activate-dmd && \
    dub build --build=release

# Expose port 8080
EXPOSE 8080

# Run the application with PGPASSWORD from environment
CMD source /root/activate-dmd && ./d-crud-api
