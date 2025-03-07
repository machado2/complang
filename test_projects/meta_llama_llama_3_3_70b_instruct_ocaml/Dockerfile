
# Use an official OCAML image
FROM opam/opam:alpine-ocaml-4.14

# Set environment variables
ENV PGPASSWORD=Saloon5-Moody-Observing
ENV DATABASE_HOST=host.docker.internal
ENV DATABASE_PORT=5432
ENV DATABASE_NAME=complang
ENV DATABASE_USER=testuser

# Install PostgreSQL driver
RUN opam install pgocaml

# Create a new directory for the application
WORKDIR /app

# Copy the application code
COPY . /app

# Expose port 8080
EXPOSE 8080

# Run the application when the container starts
CMD ["dune", "exec", "crud_api.exe"]
