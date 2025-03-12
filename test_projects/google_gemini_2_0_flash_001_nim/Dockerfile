
FROM nimlang/nim:latest AS builder
WORKDIR /app
COPY . .
RUN nimble install -y

FROM debian:stable-slim
RUN apt-get update && apt-get install -y libpq-dev postgresql-client --no-install-recommends && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY --from=builder /app/complang /app/complang
COPY --from=builder /app/config.nims /app/config.nims
ENV PGPASSWORD=Saloon5-Moody-Observing
EXPOSE 8080
CMD ["./complang"]
