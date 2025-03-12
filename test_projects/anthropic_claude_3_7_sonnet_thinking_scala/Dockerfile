FROM hseeberger/scala-sbt:11.0.15_1.6.2_2.13.8

WORKDIR /app

# Copy the build definition
COPY build.sbt ./

# Copy the source code
COPY src ./src

# Expose the application port
EXPOSE 8080

# Run the application
CMD sbt "run"
