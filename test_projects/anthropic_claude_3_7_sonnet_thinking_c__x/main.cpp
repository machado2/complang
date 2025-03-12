
#include <crow.h>
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include <pqxx/pqxx>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

// Database connection function
std::shared_ptr<pqxx::connection> connectToDatabase() {
    // Get password from environment variable
    const char* pgpassword = std::getenv("PGPASSWORD");
    if (!pgpassword) {
        throw std::runtime_error("PGPASSWORD environment variable not set");
    }
    
    std::string connStr = 
        "host=host.docker.internal "
        "port=5432 "
        "dbname=complang "
        "user=testuser "
        "password=" + std::string(pgpassword);
        
    try {
        return std::make_shared<pqxx::connection>(connStr);
    } catch (const std::exception& e) {
        std::cerr << "Connection error: " << e.what() << std::endl;
        throw;
    }
}

// User model struct
struct User {
    int id;
    std::string name;
    std::string email;
    
    json toJson() const {
        return {
            {"id", id},
            {"name", name},
            {"email", email}
        };
    }
    
    static User fromJson(const json& j) {
        User user;
        user.id = 0; // Will be assigned by database
        user.name = j["name"];
        user.email = j["email"];
        return user;
    }
};

int main() {
    crow::SimpleApp app;
    
    CROW_ROUTE(app, "/users")
        .methods(crow::HTTPMethod::GET)
        ([](const crow::request& req) {
            try {
                auto conn = connectToDatabase();
                pqxx::work txn(*conn);
                
                pqxx::result results = txn.exec("SELECT id, name, email FROM users");
                
                json response = json::array();
                for (const auto& row : results) {
                    response.push_back({
                        {"id", row[0].as<int>()},
                        {"name", row[1].as<std::string>()},
                        {"email", row[2].as<std::string>()}
                    });
                }
                
                txn.commit();
                return crow::response(200, response.dump());
            } catch (const std::exception& e) {
                return crow::response(500, std::string("Database error: ") + e.what());
            }
        });
        
    CROW_ROUTE(app, "/users")
        .methods(crow::HTTPMethod::POST)
        ([](const crow::request& req) {
            try {
                auto j = json::parse(req.body);
                if (!j.contains("name") || !j.contains("email")) {
                    return crow::response(400, "Missing required fields: name and email");
                }
                
                User user = User::fromJson(j);
                auto conn = connectToDatabase();
                pqxx::work txn(*conn);
                
                pqxx::result result = txn.exec_params(
                    "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id",
                    user.name, user.email);
                
                user.id = result[0][0].as<int>();
                txn.commit();
                
                return crow::response(201, user.toJson().dump());
            } catch (const std::exception& e) {
                return crow::response(500, std::string("Error: ") + e.what());
            }
        });
        
    CROW_ROUTE(app, "/users/<int>")
        .methods(crow::HTTPMethod::GET)
        ([](const crow::request& req, int id) {
            try {
                auto conn = connectToDatabase();
                pqxx::work txn(*conn);
                
                pqxx::result result = txn.exec_params(
                    "SELECT id, name, email FROM users WHERE id = $1",
                    id);
                
                if (result.empty()) {
                    return crow::response(404, "User not found");
                }
                
                User user;
                user.id = result[0][0].as<int>();
                user.name = result[0][1].as<std::string>();
                user.email = result[0][2].as<std::string>();
                
                txn.commit();
                return crow::response(200, user.toJson().dump());
            } catch (const std::exception& e) {
                return crow::response(500, std::string("Database error: ") + e.what());
            }
        });
        
    CROW_ROUTE(app, "/users/<int>")
        .methods(crow::HTTPMethod::PUT)
        ([](const crow::request& req, int id) {
            try {
                auto j = json::parse(req.body);
                if (!j.contains("name") || !j.contains("email")) {
                    return crow::response(400, "Missing required fields: name and email");
                }
                
                auto conn = connectToDatabase();
                pqxx::work txn(*conn);
                
                pqxx::result checkResult = txn.exec_params(
                    "SELECT id FROM users WHERE id = $1", id);
                    
                if (checkResult.empty()) {
                    return crow::response(404, "User not found");
                }
                
                txn.exec_params(
                    "UPDATE users SET name = $1, email = $2 WHERE id = $3",
                    j["name"].get<std::string>(), j["email"].get<std::string>(), id);
                
                txn.commit();
                return crow::response(200);
            } catch (const std::exception& e) {
                return crow::response(500, std::string("Error: ") + e.what());
            }
        });
        
    CROW_ROUTE(app, "/users/<int>")
        .methods(crow::HTTPMethod::DELETE)
        ([](const crow::request& req, int id) {
            try {
                auto conn = connectToDatabase();
                pqxx::work txn(*conn);
                
                pqxx::result checkResult = txn.exec_params(
                    "SELECT id FROM users WHERE id = $1", id);
                    
                if (checkResult.empty()) {
                    return crow::response(404, "User not found");
                }
                
                txn.exec_params("DELETE FROM users WHERE id = $1", id);
                txn.commit();
                
                return crow::response(204);
            } catch (const std::exception& e) {
                return crow::response(500, std::string("Error: ") + e.what());
            }
        });
    
    app.port(8080).multithreaded().run();
    return 0;
}
