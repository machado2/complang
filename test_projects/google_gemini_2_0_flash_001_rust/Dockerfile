FROM rust:1.81-slim-bookworm AS builder
WORKDIR /app

# Install dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy project files
COPY . .

# Build the application
RUN cargo build --release

# Create a minimal runtime image
FROM debian:bookworm-slim
WORKDIR /app

# Install required runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq5 \
    openssl \
    && rm -rf /var/lib/apt/lists/*

# Copy the built binary from the builder stage
COPY --from=builder /app/target/release/rust_api .

# Set environment variables
ENV DB_HOST=host.docker.internal
ENV DB_PORT=5432
ENV DB_NAME=complang
ENV DB_USER=testuser

# Expose the port
EXPOSE 8080

# Run the application
CMD [ "./rust_api" ]
