FROM ruby:2.7

WORKDIR /app

# Install dependencies
COPY Gemfile /app/
RUN bundle install

# Copy application code
COPY . /app/

# Expose the port the app runs on
EXPOSE 8080

# Command to run the application
CMD ["bundle", "exec", "ruby", "app.rb", "-o", "0.0.0.0"]
