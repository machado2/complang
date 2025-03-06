FROM ocaml/opam:ubuntu-22.04

USER root
RUN apt-get update && apt-get install -y --no-install-recommends postgresql-client && rm -rf /var/lib/apt/lists/*
USER opam

WORKDIR /app

COPY complang.opam .
RUN opam pin add . --yes
RUN opam depext -y --with-test --with-doc . && opam install -y --deps-only --locked --with-test --with-doc .

COPY . .

RUN opam build

EXPOSE 8080

CMD ["dune", "exec", "./bin/main.exe"]