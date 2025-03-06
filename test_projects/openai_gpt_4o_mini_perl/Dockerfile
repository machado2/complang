FROM perl:latest

# Install necessary Perl modules
RUN cpanm Dancer2 DBI DBD::Pg JSON

# Set the working directory
WORKDIR /app

# Copy the application files
COPY app.pl .

# Expose the port the app runs on
EXPOSE 8080

# Set the environment variable for PGPASSWORD
ENV PGPASSWORD=Saloon5-Moody-Observing

# Start the application
CMD ["perl", "app.pl"]
