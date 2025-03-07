FROM python:3.10-slim

# Set environment variables
ENV PGPASSWORD=${PGPASSWORD}

# Set working directory
WORKDIR /app

# Copy the requirements file and install dependencies
COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

# Copy the application code
COPY app.py ./

# Expose the service on port 8080
EXPOSE 8080

# Command to run the application
CMD ["python", "app.py"]
