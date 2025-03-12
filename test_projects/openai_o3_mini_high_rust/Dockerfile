
# syntax=docker/dockerfile:1
FROM rust:1.68-slim AS builder
WORKDIR /usr/src/complang_api

# Copy Cargo.toml and generate Cargo.lock for caching dependencies.
COPY Cargo.toml ./
RUN cargo generate-lockfile

# Create a dummy main file to allow fetching dependencies.
RUN mkdir src && echo "fn main() {}" > src/main.rs
RUN cargo fetch

# Copy the full source code and build the application in release mode.
COPY src ./src
RUN cargo build --release

FROM debian:buster-slim
RUN apt-get update && apt-get install -y ca-certificates && rm -rf /var/lib/apt/lists/*
# Copy the built binary from the builder.
COPY --from=builder /usr/src/complang_api/target/release/complang_api /usr/local/bin/complang_api
EXPOSE 8080
CMD ["complang_api"]
