# Use the official Gradle image to build the Kotlin application
FROM gradle:7.5.1-jdk11 AS builder

# Set the working directory
WORKDIR /app

# Copy the build.gradle.kts and settings.gradle.kts files
COPY build.gradle.kts .
COPY src ./src

# Build the application
RUN gradle build --no-daemon

# Use the official OpenJDK base image for the runtime
FROM openjdk:11-jre-slim

# Set the working directory
WORKDIR /app

# Copy the built jar file from the builder stage
COPY --from=builder /app/build/libs/*.jar app.jar

# Expose the application port
EXPOSE 8080

# Run the application
ENTRYPOINT ["java", "-jar", "app.jar"]