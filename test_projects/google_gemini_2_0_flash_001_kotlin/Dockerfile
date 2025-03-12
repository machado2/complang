
FROM eclipse-temurin:17-jre-jammy

WORKDIR /app

COPY build.gradle.kts .
COPY settings.gradle.kts .
COPY src ./src

RUN apt-get update
RUN apt-get install -y --no-install-recommends flyway-cli

# Download dependencies
RUN ./gradlew dependencies

# Build the application
RUN ./gradlew bootJar

# Expose port
EXPOSE 8080

# Set environment variable
ENV PGPASSWORD=Saloon5-Moody-Observing
# Command to run the application
ENTRYPOINT ["java", "-jar", "build/libs/kotlin-crud-0.0.1-SNAPSHOT.jar"]
