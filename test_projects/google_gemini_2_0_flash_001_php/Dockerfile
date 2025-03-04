
FROM php:8.2-cli

RUN apt-get update && apt-get install -y --no-install-recommends \
    postgresql-client \
    curl \
    libpq-dev \
    git \
    zip \
    unzip

RUN docker-php-ext-install pdo pdo_pgsql

# Install composer
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

WORKDIR /var/www/html

COPY composer.json .
RUN composer install

COPY . .

EXPOSE 8080

CMD ["php", "-S", "0.0.0.0:8080", "-t", "."]
