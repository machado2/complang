FROM node:16-alpine

# Set working directory
WORKDIR /app

# Copy dependency definitions
COPY package.json tsconfig.json ./

# Install dependencies
RUN npm install

# Copy source files
COPY src/ ./src/

# Build the project
RUN npm run build

# Expose the port the app runs on
EXPOSE 8080

# Start the app
CMD ["npm", "start"]
