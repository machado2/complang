FROM elixir:1.14-otp-25 as builder

WORKDIR /app

RUN apt-get update -yq \
  && apt-get install -yq --no-install-recommends build-essential postgresql-client \
  && rm -rf /var/lib/apt/lists/*

COPY mix.exs mix.lock ./
RUN mix deps.get

COPY config config
COPY lib lib
COPY priv priv

RUN mix compile
RUN mix phx.digest

RUN mix release

FROM alpine:3.18.2

WORKDIR /app

RUN apk add --no-cache libstdc++ postgresql-client

COPY --from=builder /app/_build/prod/rel/crud_api .

ENV PORT=8080
ENV PGPASSWORD=$PGPASSWORD
EXPOSE 8080

CMD ["/app/bin/crud_api", "start"]