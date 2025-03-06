# Use the official PHP image
FROM php:7.4-apache

# Install dependencies for PostgreSQL
RUN apt-get update && apt-get install -y libpq-dev

# Install PostgreSQL extension
RUN docker-php-ext-install pdo pdo_pgsql

# Set environment variable for the password
ENV PGPASSWORD=${PGPASSWORD}

# Configure the ServerName to suppress warning
RUN echo "ServerName localhost" >> /etc/apache2/apache2.conf

# Copy the PHP code into the image
COPY . /var/www/html/

# Set the working directory
WORKDIR /var/www/html

# Expose port 8080
EXPOSE 8080

# Start Apache
CMD ["apache2-foreground"]