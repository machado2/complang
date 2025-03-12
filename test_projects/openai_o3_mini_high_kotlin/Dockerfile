# Build stage using Gradle
FROM gradle:7.6.0-jdk11 AS builder
WORKDIR /home/gradle/project
COPY . .
RUN gradle shadowJar --no-daemon

# Run stage using a minimal JRE
FROM openjdk:11-jre-slim
WORKDIR /app
COPY --from=builder /home/gradle/project/build/libs/app.jar app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "app.jar"]
