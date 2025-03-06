FROM ubuntu:latest

RUN apt-get update && apt-get install -y \
    curl \
    raku \
    libpq-dev \
    postgresql-client \
    --no-install-recommends

# Install required Raku modules
RUN raku -v
RUN export ZEF_REPO='https://repo.raku.land'
RUN zef install --force --verbose DBIish JSON::Fast


# Copy application files
COPY . /app

WORKDIR /app

# Expose the port the app runs on
EXPOSE 8080

# Set the startup command
CMD raku app.raku