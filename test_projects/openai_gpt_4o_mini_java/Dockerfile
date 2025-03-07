
# Use the official OpenJDK image as the base image
FROM openjdk:11-jdk

# Install Maven
RUN apt-get update && apt-get install -y maven

# Set the working directory
WORKDIR /app

# Copy the pom.xml and install dependencies
COPY pom.xml .
COPY src ./src

# Run Maven package
RUN mvn package

# Expose port 8080
EXPOSE 8080

# Command to run the application
CMD ["java", "-jar", "target/complang-0.0.1-SNAPSHOT.jar"]
