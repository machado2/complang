FROM crystallang/crystal:latest as builder

WORKDIR /app

COPY shard.yml .
RUN shards install

COPY . .
RUN crystal build src/app.cr --release

FROM crystallang/crystal:latest

WORKDIR /app

COPY --from=builder /app/app .
COPY --from=builder /app/.env .env

EXPOSE 8080

CMD ["./app"]
