FROM eclipse-temurin:17-jdk
WORKDIR /app
COPY pom.xml .
RUN mvn dependency:go-offline
RUN mvn package -DskipTests
COPY target/user-api-0.0.1-SNAPSHOT.jar app.jar
ENTRYPOINT ["java","-jar","app.jar"]