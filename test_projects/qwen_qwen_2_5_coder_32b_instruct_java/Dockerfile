
FROM openjdk:17-jdk-slim

WORKDIR /app

COPY . /app

RUN apt-get update && apt-get install -y maven

RUN mvn clean package

EXPOSE 8080

CMD ["java", "-jar", "./target/app.jar"]
