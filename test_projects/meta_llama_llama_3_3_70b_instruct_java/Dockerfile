
FROM ubuntu:latest
RUN apt-get update && apt-get install -y openjdk-8-jdk maven
WORKDIR /app
COPY . .
RUN mvn clean package
EXPOSE 8080
ENTRYPOINT ["java","-jar","target/javacrud-1.0.jar"]
