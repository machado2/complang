
FROM golang:1.20-alpine AS builder

WORKDIR /app

RUN go mod init complang
COPY go.mod ./
RUN go mod download
COPY . .
RUN go build -o main .

FROM alpine:latest

WORKDIR /app

COPY --from=builder /app/main .

EXPOSE 8080

CMD ["./main"]
