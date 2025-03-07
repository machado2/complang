
FROM node:14

# Set working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY . /app

# Install any needed packages
RUN npm install

# Expose the port
EXPOSE 8080

# Run the command to start the development server
CMD [ "npm", "start" ]
