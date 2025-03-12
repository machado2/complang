
# Build stage
FROM crystallang/crystal:1.7.3-alpine AS builder

# Install build dependencies
RUN apk add --no-cache postgresql-dev

# Set the working directory
WORKDIR /app

# Copy only the dependency definition files
COPY shard.yml ./

# Install dependencies
RUN shards install

# Copy the source code
COPY src/ ./src/

# Build the application with release optimizations
RUN crystal build --release src/users-api.cr -o bin/users-api

# Runtime stage
FROM alpine:3.16

# Install runtime dependencies
RUN apk add --no-cache libpq

# Copy the compiled binary
WORKDIR /app
COPY --from=builder /app/bin/users-api .

# Expose the application port
EXPOSE 8080

# Run the application
CMD ["./users-api"]
