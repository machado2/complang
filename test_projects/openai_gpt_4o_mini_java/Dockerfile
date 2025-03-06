
# Use the official Maven image to build the application
FROM maven:3.6.3-jdk-11 AS build

# Set the working directory
WORKDIR /app

# Copy the pom.xml and the src directory with the package structure
COPY pom.xml .
COPY src ./src

# Package the application
RUN mvn clean package

# Use a lightweight JDK image to run the application
FROM openjdk:11-jre-slim

# Set the working directory
WORKDIR /app

# Copy the packaged application from the build stage
COPY --from=build /app/target/UserApi-1.0-SNAPSHOT.jar UserApi.jar

# Expose the application port
EXPOSE 8080

# Command to run the application
CMD ["java", "-jar", "UserApi.jar"]
