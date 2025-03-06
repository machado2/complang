FROM openjdk:17-jdk-slim

WORKDIR /app

COPY build/libs/complang-all.jar .

RUN ls -l

EXPOSE 8080

CMD ["java", "-jar", "complang-all.jar"]