FROM ruby:3.2

WORKDIR /app

COPY Gemfile .
RUN gem install bundler && bundle install

COPY . .

EXPOSE 8080

CMD ["ruby", "app.rb"]