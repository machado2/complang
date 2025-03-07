
FROM openjdk:8-jdk-alpine as build

# Set the working directory
WORKDIR /app

# Copy the Gradle wrapper
COPY gradlew .

# Copy the Gradle settings
COPY gradle.gradle .

# Copy the Gradle properties
COPY gradle.properties .

# Copy the Kotlin files
COPY src src

# Build the JAR file
RUN ./gradlew build

# Create a new stage for the Docker image
FROM openjdk:8-jdk-alpine

# Set the working directory
WORKDIR /app

# Copy the JAR file from the previous stage
COPY --from=build /app/build/libs/ComplangApi-0.0.1-SNAPSHOT.jar app.jar

# Expose the port
EXPOSE 8080

# Run the JAR file
ENTRYPOINT ["java","-jar","/app.jar"]
