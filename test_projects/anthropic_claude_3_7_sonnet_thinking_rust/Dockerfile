FROM rust:1.70.0 AS builder

WORKDIR /usr/src/app
COPY . .

RUN cargo build --release

FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates libssl-dev && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /usr/src/app/target/release/rust-crud-api /app/rust-crud-api

EXPOSE 8080

ENV RUST_LOG=info

CMD ["./rust-crud-api"]
