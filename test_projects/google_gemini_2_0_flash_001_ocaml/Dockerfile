
FROM ocaml/opam:debian-11-ocaml-4.14

WORKDIR /app

# Install required system dependencies
RUN sudo apt-get update && sudo apt-get install -y --no-install-recommends     libpq-dev libgmp-dev pkg-config

# Install OCaml dependencies
COPY complang.opam .
RUN opam install . --deps-only -y

# Create bin directory
RUN mkdir -p bin

# Copy the source code
COPY . .

# Build the project
RUN opam install --yes --dot-merlin .
RUN eval $(opam env) && dune build

# Expose the port
EXPOSE 8080

# Set the PGPASSWORD environment variable
ENV PGPASSWORD=Saloon5-Moody-Observing

# Run the executable
CMD ["dune", "exec", "bin/main.exe"]
