
FROM rakudo/raku:2023.01-1

COPY . /app
WORKDIR /app

# Install necessary modules using zef
RUN zef install Cro DBI JSON::Fast

CMD ["raku", "api.raku"]
