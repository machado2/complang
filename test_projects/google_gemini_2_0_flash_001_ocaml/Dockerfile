FROM ocaml/opam:debian-12-ocaml-5.1

RUN sudo apt-get update && sudo apt-get install -y --no-install-recommends \
    postgresql-client \
    libpq-dev \
    libev-dev \
    libgmp-dev \
    pkg-config

WORKDIR /app

COPY . .

RUN opam init -y
RUN opam install --yes --deps-only . --with-test --with-doc
RUN opam install --yes dream postgresql yojson

RUN eval $(opam env) && dune build -j 2

EXPOSE 8080

CMD ["dune", "exec", "main.exe"]