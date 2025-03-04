
FROM openjdk:17-jdk-slim
RUN apt-get update && apt-get install -y maven
WORKDIR /app
COPY pom.xml .
RUN mvn dependency:go-offline
COPY src ./src
RUN mvn clean install -DskipTests
COPY target/*-SNAPSHOT.jar app.jar
ENV PGPASSWORD=your_pg_password
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "app.jar"]
