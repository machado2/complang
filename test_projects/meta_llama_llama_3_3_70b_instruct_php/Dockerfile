
FROM php:7.4-apache

# Install PostgreSQL extension
RUN apt-get update && apt-get install -y libpq-dev
RUN docker-php-ext-install pgsql

# Copy the PHP script into the container
COPY api.php /var/www/html/api.php

# Configure Apache to listen on port 8080
RUN sed -i 's/80/8080/' /etc/apache2/ports.conf
RUN sed -i 's/80/8080/' /etc/apache2/sites-enabled/000-default.conf

# Expose port 8080
EXPOSE 8080

# Set environment variable for PostgreSQL password
ENV PGPASSWORD=Saloon5-Moody-Observing

# Run Apache when the container starts
CMD ["apache2-foreground"]
