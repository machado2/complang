# User API

A CRUD REST API for managing users.

## API Endpoints

- **POST /users**: Creates a user.
  - Request: `{ "name": string, "email": string }`
  - Response: `201 Created` with `{ "id": int, "name": string, "email": string }`

- **GET /users**: Returns all users.
  - Response: `200 OK` with `[{ "id": int, "name": string, "email": string }, ...]`

- **GET /users/{id}**: Returns a user by ID.
  - Response: `200 OK` with `{ "id": int, "name": string, "email": string }` or `404 Not Found`

- **PUT /users/{id}**: Updates a user.
  - Request: `{ "name": string, "email": string }`
  - Response: `200 OK` with updated user or `404 Not Found`

- **DELETE /users/{id}**: Deletes a user.
  - Response: `204 No Content` or `404 Not Found`

## Running with Docker

