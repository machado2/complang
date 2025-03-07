
# Use the official Rust image as a base
FROM rust:latest

# Set the working directory
WORKDIR /usr/src/app

# Copy the correct files to the container
COPY Cargo.toml .
COPY src ./src

# Build the application
RUN cargo build --release

# Run the application
CMD ["./target/release/crud_api"]
