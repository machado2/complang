
FROM openjdk:17-slim

WORKDIR /app

COPY project.clj .
RUN mkdir src
COPY src ./src

RUN apt-get update && apt-get install -y leiningen

RUN lein deps

RUN lein uberjar

EXPOSE 8080

CMD ["java", "-jar", "target/uberjar/complang-0.1.0-SNAPSHOT-standalone.jar"]
