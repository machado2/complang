# Use the official Ruby image
FROM ruby:latest

# Set the working directory
WORKDIR /usr/src/app

# Install PostgreSQL client
RUN apt-get update -y && apt-get install -y libpq-dev build-essential

# Copy the Gemfile and config.ru
COPY Gemfile ./
COPY config.ru ./

# Install dependencies
RUN bundle install

# Copy the rest of the application code
COPY . .

# Expose the port the app runs on
EXPOSE 8080

# Command to run the application
CMD ["rackup", "-o", "0.0.0.0", "-p", "8080"]