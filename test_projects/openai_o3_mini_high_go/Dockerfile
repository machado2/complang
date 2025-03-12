# Build stage
FROM golang:1.20 AS builder
WORKDIR /app
COPY go.mod .
# Create an empty go.sum to ensure its generation, then download dependencies and tidy up
RUN touch go.sum && go mod download && go mod tidy
COPY . .
RUN CGO_ENABLED=0 GOOS=linux go build -o app .

# Final stage
FROM alpine:latest
RUN apk --no-cache add ca-certificates
WORKDIR /root/
COPY --from=builder /app/app .
EXPOSE 8080
CMD ["./app"]
