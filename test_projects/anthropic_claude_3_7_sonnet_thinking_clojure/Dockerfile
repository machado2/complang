FROM clojure:openjdk-11-lein-2.9.5-alpine AS builder

WORKDIR /app

# Copy project definition and download dependencies
COPY project.clj /app/
RUN lein deps

# Copy source and build uberjar
COPY src /app/src
RUN lein uberjar

# Runtime image
FROM openjdk:11-jre-slim

WORKDIR /app
COPY --from=builder /app/target/users-api-standalone.jar /app/app.jar

EXPOSE 8080

CMD ["java", "-jar", "app.jar"]
