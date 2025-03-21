FROM ubuntu:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    unzip \
    wget \
    software-properties-common \
    tzdata \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install D compiler (DMD) - Following official instructions
RUN apt-get update && apt-get install -y --no-install-recommends ldc dub

ENV TERM=xterm

WORKDIR /app
RUN dub init

# Install vibe.d
RUN dub add vibe-http:>=0.9

COPY app.d .

RUN dub build

EXPOSE 8080

CMD ["./app"]
