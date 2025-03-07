
FROM openjdk:11-jre-slim

# Install sbt
RUN apt-get update &&     apt-get install -y wget &&     wget https://github.com/sbt/sbt/releases/download/v1.5.5/sbt-1.5.5.tgz &&     tar -xvf sbt-1.5.5.tgz &&     chmod +x sbt/bin/* &&     mv sbt /usr/local/bin/ &&     rm sbt-1.5.5.tgz

WORKDIR /app

# Copy your project files
COPY . /app

# Use sbt to build your project
RUN sbt package

CMD ["java", "-jar", "target/scala-2.12/your-app.jar"]
