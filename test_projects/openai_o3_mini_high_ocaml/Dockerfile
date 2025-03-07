FROM ocaml/opam:debian

# Install system dependencies (PostgreSQL client)
RUN sudo apt-get update && sudo apt-get install -y libpq-dev

WORKDIR /app
COPY . .

# Install opam packages required by the project
RUN opam update && opam install opium yojson postgresql -y

# Build the project with dune
RUN dune build main.exe

EXPOSE 8080

CMD ["dune", "exec", "./main.exe"]
