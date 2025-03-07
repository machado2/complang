
FROM golang:1.21-alpine AS builder

WORKDIR /app

RUN go mod init example.com/users
COPY go.mod ./
RUN go mod download
RUN go mod tidy

COPY . .

RUN go build -o main .

FROM alpine:latest

WORKDIR /app

COPY --from=builder /app/main .

EXPOSE 8080

CMD ["./main"]
