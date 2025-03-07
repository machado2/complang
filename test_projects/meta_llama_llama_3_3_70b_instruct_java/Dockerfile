
FROM maven:3.6.0-jdk-8-slim AS build
COPY pom.xml /usr/src/app/
COPY main /usr/src/app/main
RUN mvn clean package

FROM openjdk:8-jdk-alpine
ARG JAR_FILE=target/complang-0.0.1-SNAPSHOT.jar
COPY --from=build /usr/src/app/target/complang-0.0.1-SNAPSHOT.jar app.jar
ENTRYPOINT ["java","-jar","/app.jar"]
EXPOSE 8080
