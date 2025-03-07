FROM ruby:3.2

# Install dependencies for building gems and the pg gem
RUN apt-get update -qq && apt-get install -y build-essential libpq-dev

WORKDIR /app

# Copy Gemfile and install dependencies
COPY Gemfile* ./
RUN bundle install

# Copy the entire application
COPY . .

# Expose port 8080
EXPOSE 8080

# Start the application using rackup with Puma (the default rackup server)
CMD ["bundle", "exec", "rackup", "-o", "0.0.0.0", "-p", "8080"]
