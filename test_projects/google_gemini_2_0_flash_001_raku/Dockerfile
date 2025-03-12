
FROM ubuntu:latest

# Install necessary dependencies
RUN apt-get update && apt-get install -y \
    raku \
    zef \
    postgresql-client \
    libpq-dev \
    --no-install-recommends

# Copy application source code
WORKDIR /app
COPY . /app

# Install Raku dependencies
RUN zef install --deps-only .

# Expose port 8080
EXPOSE 8080

# Run the application
CMD ["raku", "main.raku"]
