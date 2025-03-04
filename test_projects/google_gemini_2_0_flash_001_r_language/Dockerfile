FROM rocker/r-ver:4.2.3

RUN apt-get update -y && apt-get install -y --no-install-recommends libpq-dev libcurl4-openssl-dev

RUN apt-get update -y && R -e "install.packages(c('plumber', 'RPostgres', 'DBI'), repos='http://cran.rstudio.com/')"

WORKDIR /app
COPY . /app

EXPOSE 8080

CMD ["R", "-e", "plumber::pr_run(plumber::pr('plumber.R'), port=8080)"]
