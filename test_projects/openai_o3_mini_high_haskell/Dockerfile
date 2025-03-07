FROM haskell:latest
RUN apt-get update && apt-get install -y libpq-dev
WORKDIR /app
COPY . .
RUN cabal update && cabal build
EXPOSE 8080
CMD ["cabal", "run", "complang-api"]