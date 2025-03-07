FROM python:3.9

# Set the working directory
WORKDIR /app

# Copy the source code into the container
COPY . /app

# Install required Python packages
RUN pip install fastapi uvicorn sqlalchemy psycopg2-binary

# Expose port 8080 to the outside world
EXPOSE 8080

# Command to run the application using uvicorn
CMD ["uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8080"]
