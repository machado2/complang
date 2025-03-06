FROM haskell:latest

WORKDIR /app

COPY stack.yaml .
COPY cabal.project .
COPY src ./src
COPY app ./app

RUN stack setup
RUN stack build

EXPOSE 8080

CMD stack exec -- complang-exe