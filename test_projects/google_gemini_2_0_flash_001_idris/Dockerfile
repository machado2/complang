
FROM idris2:0.6.0

WORKDIR /app

# Install necessary dependencies (example for Debian-based systems)
RUN apt-get update && apt-get install -y libpq-dev

# Copy project files
COPY . /app

# Install Idris project dependencies (using ipkg)
RUN idris2 --install-pkg .

# Build the project
RUN idris2 --build Main.idr

# Expose port 8080
EXPOSE 8080

# Run the application
CMD ["./Main"]
