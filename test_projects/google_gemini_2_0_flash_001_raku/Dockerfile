
FROM ubuntu:latest

RUN apt-get update && apt-get install -y \
    curl \
    git \
    build-essential \
    libpq-dev \
    ca-certificates \
    --no-install-recommends

RUN update-ca-certificates

# Install Rakudo Star
RUN curl  https://rakudo.org/files/star/rakudo-star-2023.09.tar.gz -o rakudo-star.tar.gz
RUN tar -xzf rakudo-star.tar.gz
RUN cd rakudo-star-2023.09 && perl Configure.pl --gen-moar --gen-jvm --prefix=/opt/rakudo
RUN make -j $(nproc)
RUN make install

# Set up environment
ENV PATH="/opt/rakudo/bin:${PATH}"
ENV RAKULIB="/opt/rakudo/share/perl6/site/lib"

# Install required Raku modules
RUN zef install Cro::HTTP
RUN zef install JSON::Fast
RUN zef install DBIish
RUN zef install Env::PGPASSWORD

# Copy application files
WORKDIR /app
COPY app.raku .

# Expose port
EXPOSE 8080

# Run the application
CMD raku app.raku
