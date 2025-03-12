
FROM php:8.2-apache

RUN apt-get update && apt-get install -y --no-install-recommends \
    postgresql-client \
    libpq-dev \
    curl \
    zip \
    unzip

RUN echo "ServerName localhost" >> /etc/apache2/apache2.conf

RUN docker-php-ext-configure pgsql -with-pgsql=/usr/local/pgsql \
    && docker-php-ext-install pdo pdo_pgsql pgsql

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

RUN a2enmod rewrite && service apache2 restart

WORKDIR /var/www/html

COPY . /var/www/html

RUN composer install

EXPOSE 8080

CMD ["apache2-foreground"]

