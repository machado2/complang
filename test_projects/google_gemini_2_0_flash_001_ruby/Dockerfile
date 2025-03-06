FROM ruby:3.2

RUN apt-get update -qq && apt-get install -y --no-install-recommends postgresql-client

WORKDIR /app

COPY Gemfile Gemfile
COPY Gemfile.lock Gemfile.lock
RUN bundle install

COPY . .

EXPOSE 8080

CMD ["bundle", "exec", "ruby", "app.rb"]