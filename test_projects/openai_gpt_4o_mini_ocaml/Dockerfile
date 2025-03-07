
# Dockerfile
FROM ocaml/opam:debian-10-ocaml-4.12

USER root

# Install necessary libraries
RUN apt-get update && apt-get install -y --no-install-recommends     libpgocaml-dev     && opam update     && opam install cohttp-lwt pgx yojson lwt

COPY . /app
WORKDIR /app

RUN ocamlbuild -pkgs cohttp-lwt,pgx,yojson api.native

CMD ["./api.native"]
