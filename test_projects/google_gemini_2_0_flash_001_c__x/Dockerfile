
FROM ubuntu:latest

# Set environment variables
ENV PGPASSWORD=Saloon5-Moody-Observing

# Install dependencies
RUN apt-get update && apt-get install -y     build-essential     cmake     git     libpq-dev     wget     libboost-dev     libboost-system-dev     libboost-filesystem-dev     curl

# Download and install cpprestsdk - using latest release if v2.10.18 not working
RUN CPP_REST_VERSION="v2.10.19" && \
    git clone --depth 1 -b ${CPP_REST_VERSION} https://github.com/microsoft/cpprestsdk.git && \
    mkdir cpprestsdk/build && \
    cd cpprestsdk/build && \
    cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr && \
    make -j$(nproc) && \
    make install && \
    cd / && \
    rm -rf cpprestsdk

# Clone the project repository (assuming it's in the same directory)
RUN git clone . /app

# Build the application
WORKDIR /app
RUN g++ -std=c++17 -o crud_api main.cpp db.cpp user.cpp -lpq -lcpprest -pthread

# Expose port
EXPOSE 8080

# Run the application
CMD ["./crud_api"]
