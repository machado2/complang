
FROM perl:latest

WORKDIR /app

# Install dependencies
RUN apt-get update && apt-get install -y libpq-dev --no-install-recommends && rm -rf /var/lib/apt/lists/*
RUN cpanm --notest Mojolicious::Lite DBI JSON Data::Dumper Encode Encode::Detect DBD::Pg

# Copy application files
COPY app.pl .

# Expose port
EXPOSE 8080

# Run the application
CMD ["perl", "app.pl", "daemon", "-l", "http://*:8080"]
