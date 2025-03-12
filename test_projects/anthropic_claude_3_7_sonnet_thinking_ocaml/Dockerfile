FROM ocaml/opam:debian-11-ocaml-4.14

WORKDIR /app

# Install system dependencies
RUN sudo apt-get update && sudo apt-get install -y \
    libpq-dev \
    pkg-config \
    m4 \
    && sudo apt-get clean \
    && sudo rm -rf /var/lib/apt/lists/*

# Install OCaml dependencies
RUN opam update && opam install -y \
    dune \
    yojson \
    dream \
    lwt \
    pgx \
    pgx_unix \
    pgx_value \
    logs \
    fmt \
    caqti \
    caqti-lwt \
    caqti-driver-postgresql

# Copy project files
COPY . /app/

# Build the application
RUN eval $(opam env) && dune build

# Run the application
EXPOSE 8080
ENV PGPASSWORD=""
CMD eval $(opam env) && dune exec ./main.exe
