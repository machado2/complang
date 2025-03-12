FROM golang:1.20 AS builder

WORKDIR /app

# Copy only the go.mod file first to leverage Docker cache
COPY go.mod .

# Download and install dependencies
# This will create the go.sum file
RUN go mod download && go mod verify

# Copy the rest of the source code
COPY . .

# Build the application
RUN CGO_ENABLED=0 GOOS=linux go build -o api-server .

# Final lightweight image
FROM alpine:latest

WORKDIR /app

# Install necessary runtime dependencies
RUN apk --no-cache add ca-certificates tzdata

# Copy the binary from the builder stage
COPY --from=builder /app/api-server .

# Set the environment variable placeholder
ENV PGPASSWORD=""

# Expose port 8080
EXPOSE 8080

# Run the application
CMD ["./api-server"]
