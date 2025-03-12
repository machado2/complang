# Users CRUD API

A simple RESTful CRUD API for managing users using Spring Boot and PostgreSQL.

## Project Structure

- `src/main/java/com/example/usersapi/UsersApiApplication.java`: Main application class
- `src/main/java/com/example/usersapi/model/User.java`: User entity model
- `src/main/java/com/example/usersapi/repository/UserRepository.java`: JPA repository for User entities
- `src/main/java/com/example/usersapi/controller/UserController.java`: REST controller with CRUD endpoints
- `src/main/resources/application.properties`: Application configuration
- `Dockerfile`: Docker configuration to build and run the application

## API Endpoints

- `POST /users`: Create a user
- `GET /users`: Get all users
- `GET /users/{id}`: Get a specific user by ID
- `PUT /users/{id}`: Update a specific user
- `DELETE /users/{id}`: Delete a specific user

## How to Build and Run

1. Set the PGPASSWORD environment variable with the PostgreSQL password
2. Build the Docker image: `docker build -t users-api .`
3. Run the container: `docker run -p 8080:8080 -e PGPASSWORD=your_password users-api`
