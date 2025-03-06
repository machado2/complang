FROM openjdk:8-jdk-alpine

# Set the working directory inside the container
WORKDIR /app

# Copy the build definition file
COPY build.sbt .

# Pre-resolve dependencies
RUN apk update && apk add --no-cache bash
RUN apk add --no-cache wget
RUN apk add --no-cache sbt

# Copy the project definition file
COPY project/ project/

# Copy the source code
COPY src/ src/

# Build the application
RUN sbt assembly

# Expose port 8080 to the outside world
EXPOSE 8080

# Set the command to run when the container starts
CMD ["java", "-Dpgpassword=${PGPASSWORD}", "-jar", "target/scala-2.12/complangcrud-assembly-0.1.0-SNAPSHOT.jar"]