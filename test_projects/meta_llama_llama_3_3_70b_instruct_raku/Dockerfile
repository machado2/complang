
FROM raku/raku:latest

WORKDIR /app

COPY app.raku /app/app.raku

RUN raku -e 'use v6.d; use DBIish; DBIish.install-schemas()'

ENV PGPASSWORD=Saloon5-Moody-Observing

EXPOSE 8080

CMD ["raku", "app.raku"]
