FROM ocaml/opam:debian-11-ocaml-4.14

RUN sudo apt-get update && sudo apt-get install -y --no-install-recommends postgresql-client
RUN opam install dune

WORKDIR /app

RUN opam pin add complang ./complang.opam # Add the current directory as a package source

COPY . .

RUN opam install --yes --deps-only ./ # Install dependencies
RUN opam install --yes complang.opam # Install the package itself

ENV LANG=C.UTF-8

RUN eval $(opam env) && dune build ./src/main.exe

EXPOSE 8080

CMD ["dune", "exec", "./src/main.exe"]
