FROM lua:5.4-alpine

# Update package repository and install necessary packages
RUN apk update && apk add --no-cache luarocks gcc musl-dev postgresql-dev

# Install lapis (web framework) and pgmoon (PostgreSQL driver) via LuaRocks
RUN luarocks install lapis
RUN luarocks install pgmoon

# Set working directory and copy the application file
WORKDIR /app
COPY app.lua /app/app.lua

# Expose port 8080
EXPOSE 8080

# Start the Lapis server in production mode on port 8080
CMD ["lapis", "server", "production", "-p", "8080"]
