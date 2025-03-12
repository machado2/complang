FROM haskell:8-bullseye

RUN apt-get update && apt-get install -y wget ca-certificates gnupg lsb-release && \
    echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list && \
    wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - && \
    apt-get update && \
    apt-get install -y libpq-dev postgresql-client-14

WORKDIR /app
COPY . .

RUN cabal update && cabal build

EXPOSE 8080

CMD ["cabal", "run", "complang-api-exe"]
