# Use an official Node.js runtime as a parent image
FROM node:16-alpine

# Create and set the working directory
WORKDIR /app

# Copy package.json and package-lock.json (if available)
COPY package*.json ./

# Install dependencies
RUN npm install

# Copy the rest of the application code
COPY . .

# Build the TypeScript code
RUN npm run build

# Expose port 8080
EXPOSE 8080

# Start the server
CMD [ "npm", "start" ]
