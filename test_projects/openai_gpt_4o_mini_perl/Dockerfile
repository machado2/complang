FROM perl:5.32

# Install required Perl modules
RUN cpanm --notest Dancer2 DBD::Pg JSON

# Create app directory
WORKDIR /usr/src/app

# Copy the Perl script into the container
COPY app.pl .

# Set the environment variable for the PostgreSQL password
ENV PGPASSWORD=Saloon5-Moody-Observing

# Expose the port the app runs on
EXPOSE 8080

# Run the Perl application
CMD ["perl", "app.pl"]
