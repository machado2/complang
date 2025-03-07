
FROM rust:1.81-slim-bookworm as builder

WORKDIR /app

COPY Cargo.toml .
RUN cargo build --release --target x86_64-unknown-linux-gnu

COPY src ./src
COPY migrations ./migrations
RUN cargo build --release --target x86_64-unknown-linux-gnu

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends libpq-dev openssl ca-certificates && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /app/target/x86_64-unknown-linux-gnu/release/rust_crud_api .
COPY --from=builder /app/migrations ./migrations

ENV PGPASSWORD=Saloon5-Moody-Observing
ENV DATABASE_URL=postgres://testuser:$PGPASSWORD@host.docker.internal:5432/complang
ENV PORT=8080

EXPOSE 8080

CMD ["./rust_crud_api"]
