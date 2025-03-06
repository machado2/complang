# Use the official GCC image
FROM gcc:latest

# Set the working directory
WORKDIR /app

# Copy the source code into the container
COPY . /app

# Install necessary libraries
RUN apt-get update && apt-get install -y libpqxx-dev nlohmann-json3-dev && rm -rf /var/lib/apt/lists/*

# Build the C++ application
RUN g++ -o myapp main.cpp -lpqxx -lpjsoncpp -lpthread

# Expose the application port
EXPOSE 8080

# Command to run the application
CMD ["./myapp"]