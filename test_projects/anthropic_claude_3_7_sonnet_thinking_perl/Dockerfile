FROM perl:5.34

WORKDIR /app

# Install required Perl modules and PostgreSQL client
RUN apt-get update && apt-get install -y     libpq-dev     && rm -rf /var/lib/apt/lists/*     && cpanm Mojolicious     && cpanm DBI     && cpanm DBD::Pg

# Copy application code
COPY app.pl /app/

# Set permissions
RUN chmod +x /app/app.pl

# Expose the application port
EXPOSE 8080

# Run the application
CMD ["perl", "/app/app.pl"]
