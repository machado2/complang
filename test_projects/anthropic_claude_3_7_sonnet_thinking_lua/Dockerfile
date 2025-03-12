FROM alpine:3.15

# Install Lua, LuaRocks, and PostgreSQL client
RUN apk add --no-cache lua5.1 lua5.1-dev luarocks postgresql-client gcc musl-dev make curl

# Install required Lua dependencies
RUN luarocks install luasql-postgres
RUN luarocks install lua-cjson
RUN luarocks install luasocket
RUN luarocks install pegasus

# Set the working directory
WORKDIR /app

# Copy the application files
COPY app.lua /app/app.lua

# Expose port 8080
EXPOSE 8080

# Start the server
CMD ["lua", "/app/app.lua"]
