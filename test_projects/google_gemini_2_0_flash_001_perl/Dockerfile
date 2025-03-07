
FROM perl:5.34

WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev \
    build-essential \
    gcc \
    make \
    && rm -rf /var/lib/apt/lists/*

# Install Perl dependencies
RUN cpanm Mojolicious DBI DBD::Pg

# Copy application code
COPY app.pl .

# Expose port
EXPOSE 8080

# Start the application
CMD ["perl", "app.pl", "daemon", "-l", "http://*:8080"]
