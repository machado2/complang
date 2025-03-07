FROM idris-lang/idris:latest

# Install PostgreSQL client and necessary libraries
RUN apt-get update && apt-get install -y libpq-dev postgresql-client

# Set the working directory
WORKDIR /app

# Copy project files into the container
COPY . .

# Build the Idris project (assumes Main.idr is the entry point)
RUN idris Main.idr -o app

# Expose port 8080
EXPOSE 8080

# Command to run the application
CMD ["./app"]
