
FROM ubuntu:latest

# Update package lists
RUN apt-get update

# Install necessary packages
RUN apt-get install -y curl wget unzip
RUN apt-get install -y libpq-dev

# Install D compiler (dmd) - Correct the package name here
RUN apt-get install -y ldc

# Install vibe.d
RUN curl -fsS https://dlang.org/install.sh | sh -sf -d /usr/local -v

# Set environment variables
ENV PGPASSWORD=${PGPASSWORD}
ENV APP_PORT=8080

# Create app directory
WORKDIR /app

# Copy application source code
COPY . .

# Build the application
RUN dmd -O -release src/app.d -L-lpostgres -L-lpq

# Expose the port
EXPOSE $APP_PORT

# Command to run the application
CMD ["./app"]
