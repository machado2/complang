# Build Stage
FROM golang:1.20-alpine AS builder
WORKDIR /app
COPY go.mod ./
RUN apk add --no-cache git && go mod download
COPY . .
RUN CGO_ENABLED=0 GOOS=linux go build -o app .

# Run Stage
FROM alpine:latest
WORKDIR /app
COPY --from=builder /app/app .
EXPOSE 8080
CMD ["./app"]