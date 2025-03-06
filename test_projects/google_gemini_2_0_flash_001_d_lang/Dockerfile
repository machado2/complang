FROM ubuntu:latest

RUN apt-get update && apt-get install -y \
    curl \
    unzip \
    wget \
    software-properties-common \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y snapd dbus \
    && rm -rf /var/lib/apt/lists/*

# systemd is not available in docker containers by default
# You can either switch to a base image built for systemd, or avoid using systemctl
# Using a systemd-less solution for installing dmd is preferred for docker

RUN snap install dmd --classic
RUN snap install dub --classic

# Install vibe.d (or use dub to manage dependencies)
#RUN wget https://github.com/vibe-d/vibe.d/archive/refs/tags/v0.9.6.zip -O vibe.zip
#RUN unzip vibe.zip
#RUN cd vibe.d-0.9.6 && ./install.sh -f


# Set up the working directory
WORKDIR /app

# Copy the application source code
COPY . .

# Build the application using dub
RUN dub build

# Expose the port the application listens on
EXPOSE 8080

# Command to run the application
CMD ["./app"]