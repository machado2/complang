
# Use an official Node.js runtime as a parent image
FROM node:14

# Set the working directory in the container
WORKDIR /usr/src/app

# Copy package.json and package-lock.json to the working directory
COPY package.json ./
COPY tsconfig.json ./

# Install dependencies
RUN npm install

# Copy the rest of the application code
COPY src ./src

# Build the TypeScript code
RUN npm run build

# Make the container's port 8080 available to the outside
EXPOSE 8080

# Run the application
CMD ["npm", "start"]
