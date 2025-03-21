
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y curl wget git iputils-ping dnsutils traceroute net-tools luarocks postgresql-client libssl-dev libpq-dev lua5.4 liblua5.4-dev

ENV LUA_INCDIR=/usr/include/lua5.4

RUN luarocks --lua-version 5.4 install lapis
RUN git clone https://github.com/brimworks/luaposgres.git
WORKDIR /luaposgres
RUN luarocks --lua-version 5.4 install .
RUN luarocks --lua-version 5.4 install lua-json

WORKDIR /app
COPY . /app

EXPOSE 8080

CMD ["lapis", "server"]
