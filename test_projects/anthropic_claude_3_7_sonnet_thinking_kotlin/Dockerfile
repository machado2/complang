FROM gradle:7.6.1-jdk17 as build

WORKDIR /app
COPY . .
RUN gradle build -x test

FROM openjdk:17-jdk-slim

WORKDIR /app
COPY --from=build /app/build/libs/*.jar app.jar

ENV PGPASSWORD=""

EXPOSE 8080
ENTRYPOINT ["java", "-jar", "app.jar"]
