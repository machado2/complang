FROM clojure:latest

# Set the working directory
WORKDIR /app

# Copy project.clj and deps.edn
COPY project.clj deps.edn ./

# Download dependencies
RUN clojure -Sdeps "$(cat deps.edn)"

# Copy the Clojure source code
COPY src ./src

# Set environment variables
ENV PGPASSWORD=Saloon5-Moody-Observing

# Expose the port the application will run on
EXPOSE 8080

# Command to start the application
CMD ["clojure", "-m", "users.main"]