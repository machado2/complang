
FROM perl:latest

WORKDIR /app

# Install dependencies
RUN cpanm Mojolicious DBI DBD::Pg

# Copy application files
COPY app.pl .

# Expose port
EXPOSE 8080

# Start the application
CMD perl app.pl daemon -l http://*:8080
