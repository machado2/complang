
# Use the official OpenJDK image
FROM openjdk:11-jre-slim

# Set the working directory
WORKDIR /app

# Install Kotlin using the package manager
RUN apt-get update &&     apt-get install -y wget &&     wget https://raw.githubusercontent.com/JetBrains/kotlin/master/dist/kotlin-compiler-1.5.31.zip &&     unzip kotlin-compiler-1.5.31.zip &&     mv kotlinc /usr/local/bin/ &&     rm kotlin-compiler-1.5.31.zip &&     apt-get clean

# Copy all Kotlin files to the container
COPY . .

# Compile the application
RUN kotlinc Main.kt User.kt UserController.kt -cp ktor-server-core.jar:ktor-server-netty.jar:ktor-serialization-jackson.jar:kotlinx-coroutines-core.jar -d app.jar

# Run the application
CMD ["java", "-jar", "app.jar"]
