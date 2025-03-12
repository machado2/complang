
FROM ocaml/opam:ubuntu-22.04

WORKDIR /app

# Install system dependencies
RUN apt-get update -y && apt-get install -y --no-install-recommends postgresql-client libpq-dev

# Copy project files
COPY . .

# Install OCaml dependencies
RUN opam update
RUN opam install --yes --deps-only .

# Build the application
RUN eval $(opam env) && dune build ./bin/main.exe

# Expose port
EXPOSE 8080

# Run the application
CMD ["dune" "exec" "./bin/main.exe"]
