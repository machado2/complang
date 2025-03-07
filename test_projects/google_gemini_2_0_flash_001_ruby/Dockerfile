
FROM ruby:3.1

WORKDIR /app

COPY Gemfile Gemfile
RUN BUNDLE_IGNORE_CONFIG=1 bundle install

COPY . .

EXPOSE 8080

CMD ["ruby", "app.rb", "-p", "8080"]
