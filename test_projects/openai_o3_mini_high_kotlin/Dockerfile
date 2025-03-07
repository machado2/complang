# Build Stage
FROM gradle:7.5.1-jdk17 as builder
WORKDIR /home/gradle/project
COPY . .
RUN gradle build --no-daemon

# Run Stage
FROM openjdk:17-jdk-slim
WORKDIR /app
COPY --from=builder /home/gradle/project/build/libs/app.jar app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "app.jar"]