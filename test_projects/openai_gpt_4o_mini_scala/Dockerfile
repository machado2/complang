FROM openjdk:11-jre-slim

WORKDIR /app

COPY ./target/scala-2.13/*.jar app.jar

CMD ["java", "-jar", "app.jar"]

EXPOSE 8080
