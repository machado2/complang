FROM ruby:3.2-slim
WORKDIR /app
COPY Gemfile /app/Gemfile
RUN gem install bundler && bundle install
COPY . /app
EXPOSE 8080
CMD ["ruby", "app.rb"]
