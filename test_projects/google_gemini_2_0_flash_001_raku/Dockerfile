
FROM ubuntu:latest

RUN apt-get update && apt-get install -y \
    raku \
    libpq-dev \
    curl \
    make \
    gcc \
    git

RUN apt-get install -y libjson-xs-perl

RUN zef install JSON::Fast DBIish Cro::HTTP::Router Cro::HTTP::Server

WORKDIR /app

COPY app.raku .

EXPOSE 8080

CMD raku app.raku
