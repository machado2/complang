FROM debian:bullseye-slim

# Install dependencies
RUN apt-get update && \
    apt-get install -y wget build-essential postgresql-client libpq-dev && \
    wget https://ziglang.org/download/0.9.0/zig-linux-x86_64-0.9.0.tar.xz && \
    tar -xf zig-linux-x86_64-0.9.0.tar.xz && \
    mv zig-linux-x86_64-0.9.0/zig /usr/local/bin/ && \
    rm -rf zig-linux-x86_64-0.9.0 && \
    rm -rf zig-linux-x86_64-0.9.0.tar.xz

# Set the working directory
WORKDIR /app

# Copy the Zig source code
COPY api.zig .

# Build the Zig application
RUN zig build-exe api.zig -o api

# Run the application
CMD ["./api"]

# Expose the port
EXPOSE 8080
