# Use the official Node.js LTS image as the base image
FROM node:14

# Create app directory
WORKDIR /usr/src/app

# Copy package.json and install dependencies
COPY package.json ./
RUN npm install --only=production

# Copy app source code
COPY server.js ./

# Expose port 8080
EXPOSE 8080

# Start the server
CMD [ "node", "server.js" ]
