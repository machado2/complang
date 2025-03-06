FROM clojure:openjdk-11-tools-deps

WORKDIR /app

COPY src/ ./src/

RUN lein deps

CMD ["lein", "run"]

EXPOSE 8080