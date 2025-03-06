# CRUD API with TypeScript

This is a simple CRUD API built with TypeScript, Express, and PostgreSQL.

## Getting Started

1. Clone the repository.
2. Run `npm install` to install the dependencies.
3. Set up your database credentials in a `.env` file or modify the `docker-compose.yml` for the environment variables.
4. Use `docker-compose up` to start the application.

## API Endpoints

- `POST /users`: Create a new user
- `GET /users`: Get all users
- `GET /users/:id`: Get a user by ID
- `PUT /users/:id`: Update a user by ID
- `DELETE /users/:id`: Delete a user by ID