# Use the official Haskell base image
FROM haskell:8.10.7

# Set the working directory
WORKDIR /app

# Copy the entire project into the container
COPY . .

# Install stack
RUN stack setup --resolver lts-18.28

# Build the project
RUN stack build

# Expose the port the app runs on
EXPOSE 8080

# Command to run the executable
CMD ["stack", "exec", "app"]