
FROM haskell:latest

WORKDIR /app

COPY haskell-crud.cabal .
RUN cabal update
RUN cabal install --only-dependencies

COPY app app
COPY Database.hs .

RUN cabal build

EXPOSE 8080

CMD ["cabal", "run", "haskell-crud"]
