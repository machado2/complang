# Stage 1: Build the application
FROM maven:3.8.6-openjdk-11 AS build
WORKDIR /app
# Copy the Maven project file and download dependencies
COPY pom.xml .
RUN mvn dependency:go-offline -B
# Copy the source code and build the project (skip tests for speed)
COPY src ./src
RUN mvn package -DskipTests

# Stage 2: Run the application
FROM openjdk:11-jdk
WORKDIR /app
# Copy the jar from the build stage
COPY --from=build /app/target/complang-0.0.1-SNAPSHOT.jar app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "app.jar"]
