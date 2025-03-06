FROM openjdk:17-jdk-slim

WORKDIR /app

# Install curl 
RUN apt-get update && apt-get install -y curl

# Check if database is reachable before copying the jar.
RUN curl --fail host.docker.internal:5432 || exit 1

# Copy the application jar
COPY target/complang.jar app.jar

EXPOSE 8080

# During development, it is useful to pass database configuration as environment variables
ENV SPRING_DATASOURCE_URL="jdbc:postgresql://host.docker.internal:5432/complang"
ENV SPRING_DATASOURCE_USERNAME="testuser"
ENV SPRING_DATASOURCE_PASSWORD=${PGPASSWORD}

ENTRYPOINT ["java","-jar","app.jar"]