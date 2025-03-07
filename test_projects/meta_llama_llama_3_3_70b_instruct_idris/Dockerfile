FROM haskell:latest

# Install Idris
RUN apt-get update && apt-get install -y idris

# Install PostgreSQL driver
RUN idris --install qualified-postgresql

# Copy Idris source files
COPY . /app

# Set working directory to /app
WORKDIR /app

# Expose port 8080
EXPOSE 8080

# Run the command to start the API
CMD ["idris", "Main.idr", "--port", "8080"]