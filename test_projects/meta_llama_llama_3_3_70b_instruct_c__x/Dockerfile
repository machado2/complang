
FROM ubuntu:latest

# Set the working directory
WORKDIR /app

# Install the required packages
RUN apt-get update && apt-get install -y libpq-dev g++ git cmake

# Clone the pqxx repository
RUN git clone https://github.com/jtv/libpqxx.git

# Change into the pqxx directory
WORKDIR /app/libpqxx

# Checkout the latest release tag
RUN git checkout tags/7.7.4

# Create a build directory
RUN mkdir build

# Change into the build directory
WORKDIR /app/libpqxx/build

# Configure and build pqxx
RUN cmake.. && make && make install

# Change back to the app directory
WORKDIR /app

# Copy the source code
COPY. /app

# Compile the source code
RUN g++ -std=c++14 -o main main.cpp -lpqxx -lpq

# Expose the port
EXPOSE 8080

# Run the command
CMD ["./main"]
