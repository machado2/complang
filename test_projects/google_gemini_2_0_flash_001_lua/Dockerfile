
FROM ubuntu:latest

RUN apt-get update && apt-get install -y curl wget unzip

# Install LuaRocks
RUN apt-get install -y lua5.1 luarocks

# Install openssl development package
RUN apt-get install -y libssl-dev

# Install Lapis and dependencies
RUN luarocks install lapis
RUN luarocks install lua-resty-postgres
RUN luarocks install lua-cjson

# Copy the application files
WORKDIR /app
COPY app.lua .

# Expose port 8080
EXPOSE 8080

# Start the application
CMD ["lapis", "server"]
