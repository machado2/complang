FROM php:8.2-cli

WORKDIR /app

# Install dependencies
RUN apt-get update && apt-get install -y \
    postgresql-client \
    libpq-dev \
    git \
    zip \
    unzip \
    && docker-php-ext-install pdo pdo_pgsql

# Install composer
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

# Copy source code
COPY src /app/src

# Install dependencies
COPY composer.json .
RUN composer install --ignore-platform-reqs

# Move files to public directory
RUN mkdir -p public && mv /app/src/* /app/public/

# Expose port 8080
EXPOSE 8080

# Start the server
CMD ["php", "-S", "0.0.0.0:8080", "-t", "public"]