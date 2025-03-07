
# Use the official Golang image as the base image
FROM golang:1.18

# Set the Current Working Directory inside the container
WORKDIR /app

# Initialize a new Go module
RUN go mod init users

# Copy the entire source code
COPY . .

# Install the PostgreSQL driver
RUN go get github.com/lib/pq

# Build the Go application
RUN go build -o main .

# Expose port 8080 for the API
EXPOSE 8080

# Command to run the executable
CMD ["./main"]
