
FROM ruby:3.1

# Set the working directory to /app
WORKDIR /app

# Copy the Gemfile into the working directory
COPY Gemfile /app/

# Install the dependencies
RUN bundle install

# Copy the application code into the working directory
COPY. /app/

# Expose port 8080
EXPOSE 8080

# Run the application when the container starts
CMD ["rails", "server", "-b", "0.0.0.0", "-p", "8080"]
