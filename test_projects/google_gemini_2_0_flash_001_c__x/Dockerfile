FROM ubuntu:latest

RUN apt-get update && apt-get install -y \
    build-essential \
    libpq-dev \
    postgresql-client \
    curl \
    wget \
    apt-utils \
    fontconfig \
    libpqxx-dev \
    && rm -rf /var/lib/apt/lists/*

# Download and install nlohmann_json
RUN mkdir -p /usr/include/nlohmann \
    && wget https://github.com/nlohmann/json/releases/download/v3.11.2/json.hpp -O /usr/include/nlohmann/json.hpp

WORKDIR /app

COPY main.cpp .

# Compile the application
RUN g++ -std=c++17 -o myapp main.cpp -I/usr/include/pqxx  -lpqxx

EXPOSE 8080

ENV PGPASSWORD=Saloon5-Moody-Observing

CMD ["./myapp"]