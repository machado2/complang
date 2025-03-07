#include <crow.h>
#include <pqxx/pqxx>
#include <string>
#include <iostream>
#include <vector>
#include <cstdlib>

// Structure to hold user data
struct User {
    int id;
    std::string name;
    std::string email;
};

// Function to create a database connection
std::unique_ptr<pqxx::connection> create_connection() {
    // Get password from environment variable
    const char* password = std::getenv("PGPASSWORD");
    if (!password) {
        throw std::runtime_error("PGPASSWORD environment variable not set");
    }
    
    // Connection string
    std::string connection_string = 
        "host=host.docker.internal "
        "port=5432 "
        "dbname=complang "
        "user=testuser "
        "password=" + std::string(password);
    
    return std::make_unique<pqxx::connection>(connection_string);
}

// Function to convert user to JSON
crow::json::wvalue user_to_json(const User& user) {
    crow::json::wvalue json_user;
    json_user["id"] = user.id;
    json_user["name"] = user.name;
    json_user["email"] = user.email;
    return json_user;
}

int main() {
    crow::SimpleApp app;

    // POST /users - Create a new user
    CROW_ROUTE(app, "/users").methods(crow::HTTPMethod::POST)
    ([](const crow::request& req) {
        try {
            // Parse JSON body
            auto json = crow::json::load(req.body);
            
            // Validate required fields
            if (!json.has("name") || !json.has("email")) {
                return crow::response(400, "Name and email are required");
            }
            
            std::string name = json["name"].s();
            std::string email = json["email"].s();
            
            // Create database connection
            auto conn = create_connection();
            pqxx::work txn(*conn);
            
            // Insert user
            pqxx::result result = txn.exec_params(
                "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email",
                name, email
            );
            txn.commit();
            
            // Extract user data
            User user;
            user.id = result[0]["id"].as<int>();
            user.name = result[0]["name"].as<std::string>();
            user.email = result[0]["email"].as<std::string>();
            
            // Return the created user
            auto response = crow::response(201, user_to_json(user));
            response.set_header("Content-Type", "application/json");
            return response;
        } catch (const std::exception& e) {
            return crow::response(500, std::string("Internal Server Error: ") + e.what());
        }
    });

    // GET /users - Get all users
    CROW_ROUTE(app, "/users")
    ([]() {
        try {
            // Create database connection
            auto conn = create_connection();
            pqxx::work txn(*conn);
            
            // Get all users
            pqxx::result result = txn.exec("SELECT id, name, email FROM users");
            txn.commit();
            
            // Convert to JSON array
            crow::json::wvalue::list users_list;
            for (const auto& row : result) {
                User user;
                user.id = row["id"].as<int>();
                user.name = row["name"].as<std::string>();
                user.email = row["email"].as<std::string>();
                users_list.push_back(user_to_json(user));
            }
            
            crow::json::wvalue response;
            response = std::move(users_list);
            
            auto resp = crow::response(response);
            resp.set_header("Content-Type", "application/json");
            return resp;
        } catch (const std::exception& e) {
            return crow::response(500, std::string("Internal Server Error: ") + e.what());
        }
    });

    // GET /users/{id} - Get a user by ID
    CROW_ROUTE(app, "/users/<int>")
    ([](int id) {
        try {
            // Create database connection
            auto conn = create_connection();
            pqxx::work txn(*conn);
            
            // Get user by ID
            pqxx::result result = txn.exec_params(
                "SELECT id, name, email FROM users WHERE id = $1", id
            );
            txn.commit();
            
            // Check if user exists
            if (result.empty()) {
                return crow::response(404, "User not found");
            }
            
            // Extract user data
            User user;
            user.id = result[0]["id"].as<int>();
            user.name = result[0]["name"].as<std::string>();
            user.email = result[0]["email"].as<std::string>();
            
            auto response = crow::response(user_to_json(user));
            response.set_header("Content-Type", "application/json");
            return response;
        } catch (const std::exception& e) {
            return crow::response(500, std::string("Internal Server Error: ") + e.what());
        }
    });

    // PUT /users/{id} - Update a user
    CROW_ROUTE(app, "/users/<int>").methods(crow::HTTPMethod::PUT)
    ([](const crow::request& req, int id) {
        try {
            // Parse JSON body
            auto json = crow::json::load(req.body);
            
            // Validate required fields
            if (!json.has("name") || !json.has("email")) {
                return crow::response(400, "Name and email are required");
            }
            
            std::string name = json["name"].s();
            std::string email = json["email"].s();
            
            // Create database connection
            auto conn = create_connection();
            pqxx::work txn(*conn);
            
            // Check if user exists
            pqxx::result check_result = txn.exec_params(
                "SELECT 1 FROM users WHERE id = $1", id
            );
            
            if (check_result.empty()) {
                return crow::response(404, "User not found");
            }
            
            // Update user
            txn.exec_params(
                "UPDATE users SET name = $1, email = $2 WHERE id = $3",
                name, email, id
            );
            txn.commit();
            
            return crow::response(204);
        } catch (const std::exception& e) {
            return crow::response(500, std::string("Internal Server Error: ") + e.what());
        }
    });

    // DELETE /users/{id} - Delete a user
    CROW_ROUTE(app, "/users/<int>").methods(crow::HTTPMethod::DELETE)
    ([](int id) {
        try {
            // Create database connection
            auto conn = create_connection();
            pqxx::work txn(*conn);
            
            // Check if user exists
            pqxx::result check_result = txn.exec_params(
                "SELECT 1 FROM users WHERE id = $1", id
            );
            
            if (check_result.empty()) {
                return crow::response(404, "User not found");
            }
            
            // Delete user
            txn.exec_params("DELETE FROM users WHERE id = $1", id);
            txn.commit();
            
            return crow::response(204);
        } catch (const std::exception& e) {
            return crow::response(500, std::string("Internal Server Error: ") + e.what());
        }
    });

    // Start the server
    app.port(8080).multithreaded().run();
}
