
FROM golang:1.21-alpine AS builder
WORKDIR /app
ENV GO111MODULE=on
COPY . .
RUN go mod init example.com/users
RUN go mod download 
RUN go mod tidy
COPY go.sum .

RUN go build -o main .

FROM alpine:latest
WORKDIR /app
COPY --from=builder /app/main .
EXPOSE 8080
CMD ["./main"]
