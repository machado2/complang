FROM nimlang/nim:latest

WORKDIR /app
COPY . .

# Compile the Nim application in release mode.
RUN nim c -d:release --threads:on --opt:speed --out:app main.nim

EXPOSE 8080
CMD ["/app/app"]
