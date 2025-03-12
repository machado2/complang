
FROM openjdk:17-jdk-slim

ENV PGPASSWORD=Saloon5-Moody-Observing
EXPOSE 8080

WORKDIR /app

COPY pom.xml .
RUN apt-get update && apt-get install -y maven
RUN mvn dependency:go-offline

COPY src ./src

RUN mvn clean install

CMD ["java", "-jar", "target/complang-api-0.0.1-SNAPSHOT.jar"]
