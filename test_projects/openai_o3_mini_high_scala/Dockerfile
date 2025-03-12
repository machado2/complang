FROM hseeberger/scala-sbt:11.0.11_1.5.5_2.13.8 AS builder
WORKDIR /app
COPY . .
RUN sbt clean compile stage

FROM openjdk:11-jre-slim
WORKDIR /app
COPY --from=builder /app/target/universal/stage /app/
EXPOSE 8080
CMD ["bin/complang-crud-api"]
