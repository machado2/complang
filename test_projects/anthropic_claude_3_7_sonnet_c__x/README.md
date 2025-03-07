# CRUD API with C++ and PostgreSQL

A simple CRUD API that connects to a PostgreSQL database using C++ with the Crow framework.

## Requirements

- Docker
- PostgreSQL database running at host.docker.internal:5432
- Database named 'complang'
- User 'testuser' with password set in PGPASSWORD environment variable

## Building and Running

## API Endpoints

- **POST /users** - Create a user
  - Request: `{ "name": "John Doe", "email": "john@example.com" }`
  - Response: `{ "id": 1, "name": "John Doe", "email": "john@example.com" }`

- **GET /users** - Get all users
  - Response: `[{ "id": 1, "name": "John Doe", "email": "john@example.com" }, ...]`

- **GET /users/{id}** - Get a user by ID
  - Response: `{ "id": 1, "name": "John Doe", "email": "john@example.com" }`

- **PUT /users/{id}** - Update a user
  - Request: `{ "name": "Jane Doe", "email": "jane@example.com" }`
  - Response: No content (204)

- **DELETE /users/{id}** - Delete a user
  - Response: No content (204)
