
FROM elixir:1.14

WORKDIR /app

RUN mix local.hex --force && \
    mix local.rebar --force

COPY mix.exs ./
RUN mix deps.get
COPY mix.lock ./

ENV PGPASSWORD=${PGPASSWORD}

COPY . .

EXPOSE 8080

CMD ["mix", "phx.server"]
