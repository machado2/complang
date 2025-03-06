
FROM perl:latest

# Install necessary packages
RUN apt-get update -y && apt-get install -y --no-install-recommends libpq-dev build-essential cpanminus && rm -rf /var/lib/apt/lists/*

# Install Perl modules without tests to speed up the process
RUN cpanm --force --notest Dancer2 DBI DBD::Pg JSON::XS

# Set working directory
WORKDIR /usr/src/app

# Copy application files
COPY . /usr/src/app

# Set environment variables
ENV PGPASSWORD=Saloon5-Moody-Observing

# Expose port
EXPOSE 8080

# Start the application
CMD ["perl", "app.pl"]
