FROM rakudo-star
RUN apt-get update && apt-get install -y libssl-dev
RUN zef install Cro::HTTP::Router DBIish JSON::Fast --/test
WORKDIR /app
COPY . .
EXPOSE 8080
CMD ["raku", "app.raku"]
