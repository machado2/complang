# Users CRUD API

A simple CRUD API for managing users data.

## Setup

1. Clone the repository
2. Build the Docker image:
   ```
   docker build -t users-api .
   ```
3. Run the container:
   ```
   docker run -p 8080:8080 -e PGPASSWORD=<your-postgres-password> users-api
   ```

## API Endpoints

- `POST /users`: Create a new user
  - Request body: `{ "name": string, "email": string }`
  - Response: `{ "id": int, "name": string, "email": string }`
  - Status: 201 Created

- `GET /users`: Get all users
  - Response: Array of users `[{ "id": int, "name": string, "email": string }, ...]`
  - Status: 200 OK

- `GET /users/{id}`: Get a user by ID
  - Response: `{ "id": int, "name": string, "email": string }`
  - Status: 200 OK or 404 Not Found

- `PUT /users/{id}`: Update a user
  - Request body: `{ "name": string, "email": string }`
  - Response: `{ "id": int, "name": string, "email": string }`
  - Status: 200 OK or 404 Not Found

- `DELETE /users/{id}`: Delete a user
  - Response: `{ "message": "User deleted successfully" }`
  - Status: 200 OK or 404 Not Found
