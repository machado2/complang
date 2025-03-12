FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y idris && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY . .
RUN idris Main.idr -o server
EXPOSE 8080
CMD ["./server"]
