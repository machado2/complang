
FROM openjdk:8-jdk-slim

# Install sbt
RUN apt-get update
RUN apt-get install -y apt-transport-https curl gnupg
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_extras.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2669E30526BA9F" | apt-key add
RUN apt-get update
RUN apt-get install -y sbt

# Set working directory
WORKDIR /app

# Copy source code
COPY . /app

# Expose port
EXPOSE 8080

# Run the application
CMD ["sbt", "run"]
