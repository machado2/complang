
FROM alpine:latest AS builder

RUN apk update && apk add zig

WORKDIR /app

COPY . .

RUN zig build -Drelease-safe

FROM alpine:latest

WORKDIR /app

COPY --from=builder /app/zig-out/bin/crud-api .

EXPOSE 8080

CMD ["./crud-api"]
