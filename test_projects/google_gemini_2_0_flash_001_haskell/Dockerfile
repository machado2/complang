
FROM haskell:latest

WORKDIR /app

COPY . .

RUN apt-get update && apt-get install -y libpq-dev
RUN stack setup --install-ghc
RUN stack build --flag postgresql-libpq:-use-pkg-config

EXPOSE 8080

CMD ["stack", "exec", "haskell-crud-api-exe"]
