
FROM openjdk:17-slim

ENV LEIN_VERSION 2.9.10
RUN apt-get update && apt-get install -y wget gnupg
RUN wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -O /usr/local/bin/lein
RUN chmod +x /usr/local/bin/lein

WORKDIR /app
COPY project.clj .
RUN lein deps

COPY src ./src

RUN lein clean
RUN lein uberjar

EXPOSE 8080
CMD ["java", "-jar", "target/uberjar/crud-api-0.1.0-SNAPSHOT-standalone.jar"]
