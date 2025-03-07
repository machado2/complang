FROM ruby:2.7

# Set the working directory
WORKDIR /usr/src/app

# Install required gems
COPY Gemfile ./
RUN bundle install

# Copy application code
COPY . .

# Set environment variables
ENV PGPASSWORD=$PGPASSWORD

# Expose the port the app runs on
EXPOSE 8080

# Command to run the app
CMD ["ruby", "app.rb"]
