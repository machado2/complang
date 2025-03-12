# Use the official OCaml opam image as builder
FROM ocaml/opam:debian-11-ocaml-4.14 AS builder

# Install system dependencies for PostgreSQL
RUN sudo apt-get update && sudo apt-get install -y libpq-dev

# Set the working directory and copy source files
WORKDIR /src
COPY . .

# Update opam repository and install project dependencies
RUN opam update && opam install --deps-only -y .

# Build the project using dune in release mode
RUN dune build --profile=release -x

# Create a minimal runtime image
FROM debian:bullseye-slim
RUN apt-get update && apt-get install -y libpq5 && rm -rf /var/lib/apt/lists/*
WORKDIR /app

# Copy the built executable from the builder stage.
# This assumes the executable is built at _build/install/default/bin/server.exe
COPY --from=builder /src/_build/install/default/bin/server.exe .

EXPOSE 8080
CMD ["./server.exe"]
