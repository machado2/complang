FROM openjdk:17-jdk-slim
RUN apt-get update && apt-get install -y maven
COPY . /app
WORKDIR /app
RUN mvn install
RUN cp target/*.jar app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "app.jar"]