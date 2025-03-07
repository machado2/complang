
FROM golang:1.17-alpine

WORKDIR /app

RUN go mod init main
RUN go mod tidy
RUN go get github.com/lib/pq
RUN go mod download

COPY . .

RUN go build -o main .

EXPOSE 8080

CMD ["./main"]
