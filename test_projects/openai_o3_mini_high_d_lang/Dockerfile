# Use the official D language image with the LDC compiler.
FROM dlang/ldc:latest

# Set the working directory.
WORKDIR /app

# Copy project files.
COPY dub.json .
COPY source/ source/

# Build the project in release mode.
RUN dub build --build=release -b ldc2

# Expose port 8080.
EXPOSE 8080

# Run the compiled binary.
CMD ["./crud-api"]
