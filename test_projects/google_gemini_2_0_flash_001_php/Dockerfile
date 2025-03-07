FROM php:8.1-apache

RUN apt-get update && apt-get install -y --no-install-recommends \
    postgresql-client \
    libpq-dev \
    && docker-php-ext-install pdo pdo_pgsql

# Copy application files
COPY . /var/www/html/

# Copy virtual host config
COPY vhost.conf /etc/apache2/sites-available/complang.conf

# Enable site and disable default
RUN a2ensite complang.conf && a2dissite 000-default.conf

# Set environment variables
ENV PGPASSWORD=Saloon5-Moody-Observing

# Expose port 8080
EXPOSE 8080

# Reload apache to apply the changes
RUN service apache2 reload