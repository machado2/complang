# Use the official PHP image
FROM php:7.4-cli

# Install PostgreSQL development packages and PDO extensions
RUN apt-get update && apt-get install -y libpq-dev && docker-php-ext-install pdo pdo_pgsql

# Copy the PHP API script into the container
COPY index.php /var/www/html/index.php

# Set the working directory
WORKDIR /var/www/html

# Expose port 8080
EXPOSE 8080

# Run the PHP built-in web server
CMD ["php", "-S", "0.0.0.0:8080", "index.php"]
