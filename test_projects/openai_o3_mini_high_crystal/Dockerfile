
FROM crystallang/crystal:latest

# Install zlib development package
RUN apt-get update && apt-get install -y libz-dev

WORKDIR /app
COPY shard.yml .
COPY app.cr .
RUN shards install
RUN crystal build --release app.cr -o app
EXPOSE 8080
CMD ["./app"]
