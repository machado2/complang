FROM gcc:latest

# Install only essential libraries
RUN apt-get update &&     apt-get install -y g++ libpqxx-dev libboost-system-dev &&     apt-get clean

# Create the working directory
WORKDIR /usr/src/app

# Copy the source code
COPY main.cpp .

# Build the application
RUN g++ -o user_api main.cpp -lpqxx -lpq -lboost_system

# Expose the listening port
EXPOSE 8080

# Command to run the application
CMD ["./user_api"]