FROM openjdk:11-jre-slim

# Create app directory
WORKDIR /app

# Copy the application jar to the container
COPY target/myapp.jar ./myapp.jar

# Expose the application port
EXPOSE 8080

# Run the application
ENTRYPOINT ["java", "-jar", "myapp.jar"]