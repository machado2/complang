FROM rakudo-star
WORKDIR /app
COPY app.raku .
# Install required modules
RUN zef install Cro
RUN zef install DBIish
RUN zef install JSON::Fast
EXPOSE 8080
CMD ["raku", "app.raku"]
