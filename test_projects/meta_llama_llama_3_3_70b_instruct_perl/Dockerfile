
# Use the official Perl image as the base
FROM perl:latest

# Install the Plack module
RUN cpanm Plack
RUN cpanm DBI
RUN cpanm DBD::Pg

# Set the working directory to /app
WORKDIR /app

# Copy the api.pl file into the container
COPY api.pl /app/api.pl

# Expose port 8080
EXPOSE 8080

# Run the command to start the API when the container launches
CMD ["perl", "api.pl"]
