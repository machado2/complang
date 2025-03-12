# Perl CRUD API with PostgreSQL

A simple CRUD API built with Perl (Mojolicious) that connects to a PostgreSQL database.

## Requirements

- Docker
- PostgreSQL database (configured as described below)

## Database Configuration

The application expects to connect to a PostgreSQL database with the following configuration:
- Host: host.docker.internal:5432
- Database: complang
- User: testuser
- Password: Set as environment variable PGPASSWORD

The database should have a table named 'users' with the following structure:

## Building and Running the API

1. Build the Docker image:

2. Run the container:

## API Endpoints

- **POST /users**: Create a new user
  - Request body: `{ "name": "string", "email": "string" }`
  - Response: 201 Created with `{ "id": int, "name": "string", "email": "string" }`

- **GET /users**: Get all users
  - Response: 200 OK with `[{ "id": int, "name": "string", "email": "string" }, ...]`

- **GET /users/{id}**: Get a specific user by ID
  - Response: 200 OK with `{ "id": int, "name": "string", "email": "string" }` or 404 Not Found

- **PUT /users/{id}**: Update a user
  - Request body: `{ "name": "string", "email": "string" }`
  - Response: 204 No Content or 404 Not Found

- **DELETE /users/{id}**: Delete a user
  - Response: 204 No Content or 404 Not Found
