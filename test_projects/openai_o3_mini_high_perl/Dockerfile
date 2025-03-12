FROM perl:5.34

# Install cpanminus and required Perl modules
RUN apt-get update && apt-get install -y cpanminus && apt-get clean
RUN cpanm Mojolicious DBI DBD::Pg

# Copy application code
COPY app.pl /app.pl

EXPOSE 8080

CMD ["perl", "/app.pl"]
