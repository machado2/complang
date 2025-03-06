
FROM openjdk:17-jdk-slim

WORKDIR /app

COPY pom.xml .
COPY src ./src

# Install Maven and necessary packages
RUN apt-get update && apt-get install -y maven

# Build the project
RUN mvn clean package -DskipTests

EXPOSE 8080

CMD ["java", "-jar", "./target/app-1.0-SNAPSHOT.jar"]
