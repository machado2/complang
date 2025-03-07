
FROM clojure:openjdk-11-lein

WORKDIR /usr/src/app

COPY project.clj ./
COPY src ./src

RUN lein deps

CMD ["lein", "run"]
