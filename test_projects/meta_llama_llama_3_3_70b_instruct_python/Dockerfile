
FROM python:3.9-slim

# Set working directory to /app
WORKDIR /app

# Copy requirements file
COPY requirements.txt .

# Install dependencies
RUN pip install --no-cache-dir -r requirements.txt

# Copy application code
COPY . .

# Expose port 8080
EXPOSE 8080

# Set PGHOST environment variable
ENV PGHOST host.docker.internal

# Run command to start the development server
CMD ["python", "app.py"]
