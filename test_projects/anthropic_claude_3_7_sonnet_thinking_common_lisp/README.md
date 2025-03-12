# Common Lisp CRUD API

A simple CRUD API built with Common Lisp (SBCL) that connects to a PostgreSQL database.

## Overview

This application provides a RESTful API for managing users with the following endpoints:
- `GET /users` - List all users
- `GET /users/{id}` - Get a specific user
- `POST /users` - Create a new user
- `PUT /users/{id}` - Update an existing user
- `DELETE /users/{id}` - Delete a user

## Requirements

- Docker
- A running PostgreSQL database accessible at host.docker.internal:5432 with:
  - Database name: 'complang'
  - Username: 'testuser'
  - Password: Set via PGPASSWORD environment variable

## Building and Running

1. Build the Docker image:
   ```bash
   docker build -t lisp-crud-api .
   ```

2. Run the container:
   ```bash
   docker run -p 8080:8080 -e PGPASSWORD=your_password lisp-crud-api
   ```

## API Usage Examples

### Create a user

### Get all users

### Get a specific user

### Update a user

### Delete a user

