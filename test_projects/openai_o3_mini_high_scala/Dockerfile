# Stage 1: Build the application using sbt
FROM hseeberger/scala-sbt:11.0.11_1.5.5_2.13.10 AS builder
WORKDIR /app
COPY . .
RUN sbt stage

# Stage 2: Use a slim OpenJDK image for runtime
FROM openjdk:11-jre-slim
WORKDIR /app
COPY --from=builder /app/target/universal/stage /app
EXPOSE 8080
CMD ["bin/crud-api"]
