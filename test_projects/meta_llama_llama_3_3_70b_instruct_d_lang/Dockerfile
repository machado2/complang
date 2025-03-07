
# Use the official D lang image as the base
FROM docker.io/library/ubuntu:latest

# Install the D lang compiler
RUN apt update && apt install -y gcc g++ clang make libssl-dev libssl1.1

# Install the PostgreSQL driver
RUN apt install -y libpq-dev

# Set the working directory to /app
WORKDIR /app

# Copy the requirements file
COPY . /app

# Install the D lang dependencies
RUN apt install -y dub

# Build the API
RUN dub init vibe.d --name=api
RUN dub add vibe-d:postgres

# Expose the port
EXPOSE 8080

# Run the API when the container starts
CMD ["dub", "run", "--", "-p", "8080"]
