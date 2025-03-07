
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y     g++     libpq-dev     libboost-dev     libboost-system-dev     libboost-thread-dev     libcurl4-openssl-dev     cmake

# Install cpprestsdk (Casablanca)
RUN apt-get install -y libcpprest-dev

WORKDIR /app
COPY . /app

RUN mkdir build
WORKDIR /app/build
RUN cmake ..
RUN make

EXPOSE 8080
CMD ["./user_api"]
