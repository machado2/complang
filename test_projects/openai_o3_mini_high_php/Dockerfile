FROM php:8.1-cli

# Install dependencies required for PostgreSQL
RUN apt-get update && apt-get install -y libpq-dev     && docker-php-ext-install pdo_pgsql

WORKDIR /var/www/html

COPY . .

EXPOSE 8080

CMD ["php", "-S", "0.0.0.0:8080"]
