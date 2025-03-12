FROM php:8-apache

# Install PostgreSQL PDO driver
RUN apt-get update && apt-get install -y libpq-dev     && docker-php-ext-install pdo pdo_pgsql

# Enable Apache mod_rewrite
RUN a2enmod rewrite

# Configure Apache to listen on port 8080
RUN sed -i 's/Listen 80/Listen 8080/g' /etc/apache2/ports.conf
RUN sed -i 's/<VirtualHost \*:80>/<VirtualHost \*:8080>/g' /etc/apache2/sites-available/000-default.conf

# Create a custom Apache configuration file that allows .htaccess
RUN echo '<Directory /var/www/html>\n    Options Indexes FollowSymLinks\n    AllowOverride All\n    Require all granted\n</Directory>' > /etc/apache2/conf-available/htaccess.conf

# Enable the custom configuration
RUN a2enconf htaccess

# Copy application files
COPY . /var/www/html/

# Expose port 8080
EXPOSE 8080
