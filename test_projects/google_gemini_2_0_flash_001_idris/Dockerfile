
FROM ubuntu:latest

RUN apt-get update && apt-get install -y \
    software-properties-common \
    curl \
    gnupg \
    zlib1g-dev

# Install build dependencies - cabal-install, then set up a local cabal environment.
RUN apt-get update && apt-get install -y git make libgmp-dev cabal-install

# Set up a local Cabal environment
RUN mkdir /opt/cabal
ENV CABAL_DIR=/opt/cabal
ENV PATH="/opt/cabal/bin:${PATH}"

RUN cabal update
RUN cabal install cabal-install --install-method=copy --prefix=/opt/cabal

# Install idris-db dependencies - required before building Idris
RUN apt-get install -y libpq-dev

# Install Idris directly from source

RUN git clone https://github.com/idris-lang/Idris-dev.git /tmp/idris-dev
WORKDIR /tmp/idris-dev

# Install cabal dependencies required by Idris
RUN /opt/cabal/bin/cabal install --only-dependencies -j1 --disable-tests

RUN make install

# Install idris-db
RUN /opt/cabal/bin/cabal install idris-db --disable-tests

WORKDIR /app

COPY complang-api.ipkg .
COPY Main.idr .

RUN /opt/cabal/bin/idris --build complang-api.ipkg

EXPOSE 8080

CMD ["./complang-api"]
