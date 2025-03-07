FROM haskell:8.4

WORKDIR /app

COPY . .

RUN cabal update
RUN cabal install --only-dependencies
RUN cabal build

EXPOSE 8080

CMD ["dist/build/complang/complang"]
