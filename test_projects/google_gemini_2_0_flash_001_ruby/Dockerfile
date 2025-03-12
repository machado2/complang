FROM ruby:3.1

WORKDIR /app

COPY Gemfile .
RUN bundle install

COPY . .

EXPOSE 8080

CMD ["rackup", "-p", "8080", "-o", "0.0.0.0"]
