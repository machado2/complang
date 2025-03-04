FROM rust:1.81 as builder

WORKDIR /app
COPY . .
RUN cargo build --release

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends libpq-dev openssl ca-certificates iproute2 && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY --from=builder /app/target/release/rust_crud_api .

ENV PGPASSWORD=yourpassword
ENV DATABASE_URL=postgres://postgres:yourpassword@172.17.0.1:5432/test_google_gemini_2_0_flash_001_rust
EXPOSE 8080
CMD ["./rust_crud_api"]
