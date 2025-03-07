FROM perl:5.32

# Install required Perl modules
RUN cpanm --notest Mojolicious DBI DBD::Pg

# Set working directory
WORKDIR /app

# Copy the application
COPY app.pl /app/app.pl

# Make app.pl executable
RUN chmod +x /app/app.pl

# Expose port 8080
EXPOSE 8080

# Start the application
CMD ["perl", "/app/app.pl"]
