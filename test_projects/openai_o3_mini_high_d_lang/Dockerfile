# Stage 1: Build stage using Ubuntu and install DMD and Dub
FROM ubuntu:22.04 AS builder
RUN apt-get update && apt-get install -y wget tar unzip build-essential libcurl4-openssl-dev libpq-dev
RUN wget -qO- https://dlang.org/install.sh | bash -s dmd
ENV PATH="/root/dlang/dmd2/linux/bin64:${PATH}"
WORKDIR /app
COPY . .
RUN dub build --build=release

# Stage 2: Runtime stage - minimal Ubuntu image with libpq
FROM ubuntu:22.04
RUN apt-get update && apt-get install -y libpq5
WORKDIR /app
COPY --from=builder /app/complang_api .
EXPOSE 8080
CMD ["./complang_api"]
