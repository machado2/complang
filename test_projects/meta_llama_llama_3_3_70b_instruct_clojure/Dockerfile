FROM clojure:openjdk-11

# Set the working directory to /app
WORKDIR /app

# Copy the project.clj file into the working directory
COPY project.clj /app/

# Copy the src directory into the working directory
COPY src /app/src

# Install the dependencies
RUN clojure -Mdeps

# Expose the port
EXPOSE 8080

# Set the environment variable for the PostgreSQL password
ENV PGPASSWORD=Saloon5-Moody-Observing

# Run the command to start the development server when the container launches
CMD ["clojure" "-M" "complang.core"]