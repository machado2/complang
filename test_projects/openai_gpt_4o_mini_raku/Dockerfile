FROM rakudo-star

# Set working directory
WORKDIR /app

# Copy the Raku API file into the container
COPY api.raku .

# Install required dependencies
RUN zef install Cro DBI

# Set the environment variable for PostgreSQL password
ENV PGPASSWORD=Saloon5-Moody-Observing

# Expose the port
EXPOSE 8080

# Command to run the Raku application
CMD [ "rakudo", "api.raku" ]