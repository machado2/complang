# CRUD API

This service implements a CRUD API for managing users in a PostgreSQL database.

## Endpoints

- **POST /users**: Creates a user. Expects JSON `{ "name": string, "email": string }`.
- **GET /users**: Returns a list of all users.
- **GET /users/{id}**: Returns a single user by id.
- **PUT /users/{id}**: Updates a user.
- **DELETE /users/{id}**: Deletes a user.
