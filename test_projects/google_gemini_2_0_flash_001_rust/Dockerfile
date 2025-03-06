FROM rust:1.72

WORKDIR /app

COPY Cargo.toml .
COPY src ./src

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

RUN cargo build --release

ENV PGPASSWORD=Saloon5-Moody-Observing
ENV DATABASE_URL=postgres://testuser:$PGPASSWORD@host.docker.internal:5432/complang

EXPOSE 8080

CMD [ "./target/release/crud_api" ]