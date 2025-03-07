FROM maven:3.8-openjdk-11 as build

WORKDIR /app

# Copy the project files
COPY . .

# Build the application
RUN mvn clean package -DskipTests

# Runtime image
FROM openjdk:11-jre-slim

WORKDIR /app

# Copy the built jar file from the build stage
COPY --from=build /app/target/*.jar app.jar

# Expose port 8080
EXPOSE 8080

# Run the application with environment variable
CMD ["java", "-Dspring.datasource.password=${PGPASSWORD}", "-jar", "app.jar"]
