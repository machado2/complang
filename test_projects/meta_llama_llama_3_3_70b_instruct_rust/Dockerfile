
# Use the official Rust image as the base
FROM rust:alpine

# Set the working directory to /app
WORKDIR /app

# Copy the Cargo.toml file
COPY Cargo.toml.

# Install the dependencies
RUN cargo build --release

# Copy the rest of the application code
COPY..

# Expose the port
EXPOSE 8080

# Run the command to start the development server
CMD ["cargo", "run"]
