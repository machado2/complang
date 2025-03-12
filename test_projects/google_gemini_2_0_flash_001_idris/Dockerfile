FROM ubuntu:latest

# Install dependencies
RUN apt-get update && apt-get install -y \
    wget \
    tar \
    make \
    gcc \
    g++ \
    libpq-dev \
    postgresql-client

# Download and install Idris2
RUN wget https://github.com/idris-lang/Idris2/releases/download/v0.6.0/idris2-0.6.0-linux.tar.gz
RUN tar -xzf idris2-0.6.0-linux.tar.gz
RUN mv idris2-0.6.0 /opt/idris2
RUN ln -s /opt/idris2/idris2 /usr/local/bin/idris2
RUN ln -s /opt/idris2/lib /usr/local/lib/idris2

# Set environment variables
ENV IDRIS2_HOME /opt/idris2
ENV PATH "$PATH:/opt/idris2"

WORKDIR /app

# Copy project files
COPY . .

# Build the project
RUN idris2 --build main.ipkg

# Expose port
EXPOSE 8080

# Run the application
CMD ["./main"]
