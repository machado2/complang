# Use a multi-stage build to compile the Rust application
FROM rust:1.72 as builder

WORKDIR /app
# Copy Cargo.toml only (Cargo.lock is not available)
COPY Cargo.toml .

# Create a dummy main.rs to allow dependency compilation caching
RUN mkdir src
RUN echo "fn main() {}" > src/main.rs

# Build dependencies to cache them
RUN cargo build --release

# Copy the full source code
COPY . .

# Build the actual application
RUN cargo build --release

# Runtime image
FROM debian:buster-slim
RUN apt-get update && apt-get install -y libssl1.1 ca-certificates && rm -rf /var/lib/apt/lists/*
COPY --from=builder /app/target/release/complang-api /usr/local/bin/complang-api
EXPOSE 8080
CMD ["/usr/local/bin/complang-api"]
