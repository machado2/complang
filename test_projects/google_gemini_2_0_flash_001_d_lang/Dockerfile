FROM ubuntu:latest

RUN apt-get update && apt-get install -y \
    curl \
    unzip \
    libpq-dev \
    xz-utils \
    gnupg \
    && rm -rf /var/lib/apt/lists/*

# Install D compiler (DMD) - using the installer script
RUN curl -fsS https://dlang.org/install.sh | bash -s dmd

# Set up environment variables
ENV PATH="/usr/local/dmd/bin:${PATH}"
ENV LIBRARY_PATH="/usr/local/dmd/lib:${LIBRARY_PATH}"

WORKDIR /app

COPY dub.json .
COPY src ./src

# Activate D environment so dub can be found
RUN source /root/dlang/dmd-*/activate

RUN dub fetch
RUN dub build

EXPOSE 8080

CMD ["./complang_api"]