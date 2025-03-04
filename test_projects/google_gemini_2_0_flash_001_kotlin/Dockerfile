
FROM openjdk:11-jdk-slim AS builder

WORKDIR /app
COPY . /app

RUN apt-get update && apt-get install -y dos2unix curl unzip
RUN dos2unix gradlew

RUN curl -L "https://services.gradle.org/distributions/gradle-7.6.1-bin.zip" -o gradle.zip
RUN unzip gradle.zip
ENV GRADLE_HOME=/app/gradle-7.6.1
ENV PATH=$PATH:$GRADLE_HOME/bin

RUN chmod +x gradlew
RUN ./gradlew build --info

FROM openjdk:11-jre-slim

EXPOSE 8080

COPY --from=builder /app/build/libs/app.jar /app.jar

ENTRYPOINT ["java", "-jar", "/app.jar"]
