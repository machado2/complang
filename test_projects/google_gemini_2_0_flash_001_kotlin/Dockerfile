
FROM openjdk:17-jdk-slim
WORKDIR /app
COPY . .
RUN chmod +x gradlew
RUN ./gradlew build -x test
COPY build/libs/*.jar app.jar
ENV PGPASSWORD=Saloon5-Moody-Observing
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "app.jar"]
