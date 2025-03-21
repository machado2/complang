FROM crystallang/crystal:latest AS builder

WORKDIR /app

COPY shard.yml .
RUN shards install

COPY . .
RUN crystal build src/complang.cr --release

FROM crystallang/crystal:latest

WORKDIR /app

COPY --from=builder /app/complang .

EXPOSE 8080
ENV PGPASSWORD=Saloon5-Moody-Observing
CMD ["./complang"]
