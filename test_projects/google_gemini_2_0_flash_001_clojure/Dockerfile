
FROM openjdk:17-jdk-slim

RUN apt-get update && apt-get install -y leiningen

WORKDIR /app

COPY project.clj .
RUN lein deps

COPY src ./src

RUN lein compile

EXPOSE 8080

CMD ["lein", "run"]
