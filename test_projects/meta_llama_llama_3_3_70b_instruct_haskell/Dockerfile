
FROM haskell:8.10

WORKDIR /app

COPY. /app

RUN cabal update && cabal install -j4

EXPOSE 8080

CMD ["cabal", "run"]
