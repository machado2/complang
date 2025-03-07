FROM clojure:lein-2.9.1

WORKDIR /app

COPY . .

RUN lein uberjar

EXPOSE 8080

CMD ["java", "-jar", "target/crud-api.jar"]