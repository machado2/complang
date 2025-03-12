FROM nimlang/nim:latest

WORKDIR /app

# Install dependencies
RUN apt-get update && apt-get install -y libpq-dev

# Create a nimble.nim file
RUN echo '# Package' > server.nimble
RUN echo 'version       = "0.1.0"' >> server.nimble
RUN echo 'author        = "API Developer"' >> server.nimble
RUN echo 'description   = "CRUD API for users"' >> server.nimble
RUN echo 'license       = "MIT"' >> server.nimble
RUN echo '' >> server.nimble
RUN echo '# Dependencies' >> server.nimble
RUN echo 'requires "nim >= 1.6.0"' >> server.nimble
RUN echo 'requires "jester >= 0.5.0"' >> server.nimble
RUN echo 'requires "db_connector"' >> server.nimble

# Install dependencies from nimble
RUN nimble install -y jester
RUN nimble install -y db_connector

# Copy source code
COPY server.nim .

# Build the project
RUN nim c -d:release server.nim

# Expose port 8080
EXPOSE 8080

# Run the application
CMD ["./server"]
