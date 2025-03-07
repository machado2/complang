#include "user_routes.h"
#include <iostream>
#include <string>

namespace UserRoutes {
    void registerRoutes(crow::App<>& app, DBHandler& db) {
        CROW_ROUTE(app, "/users")
            .methods(crow::HTTPMethod::GET)
            ([&db](const crow::request& req) {
                auto users = db.getAllUsers();
                crow::json::wvalue response = crow::json::wvalue::list();
                
                for (size_t i = 0; i < users.size(); i++) {
                    crow::json::wvalue user;
                    user["id"] = users[i].id;
                    user["name"] = users[i].name;
                    user["email"] = users[i].email;
                    response[i] = std::move(user);
                }
                
                return crow::response(200, response);
            });
        
        CROW_ROUTE(app, "/users/<int>")
            .methods(crow::HTTPMethod::GET)
            ([&db](const crow::request& req, int id) {
                auto user = db.getUserById(id);
                
                if (user.id == 0) {
                    return crow::response(404, "User not found");
                }
                
                crow::json::wvalue response;
                response["id"] = user.id;
                response["name"] = user.name;
                response["email"] = user.email;
                
                return crow::response(200, response);
            });
        
        CROW_ROUTE(app, "/users")
            .methods(crow::HTTPMethod::POST)
            ([&db](const crow::request& req) {
                auto json = crow::json::load(req.body);
                
                if (!json) {
                    return crow::response(400, "Invalid JSON");
                }
                
                if (!json.has("name") || !json.has("email")) {
                    return crow::response(400, "Missing required fields: name, email");
                }
                
                std::string name = json["name"].s();
                std::string email = json["email"].s();
                
                auto user = db.createUser(name, email);
                
                if (user.id == 0) {
                    return crow::response(500, "Failed to create user");
                }
                
                crow::json::wvalue response;
                response["id"] = user.id;
                response["name"] = user.name;
                response["email"] = user.email;
                
                return crow::response(201, response);
            });
        
        CROW_ROUTE(app, "/users/<int>")
            .methods(crow::HTTPMethod::PUT)
            ([&db](const crow::request& req, int id) {
                auto json = crow::json::load(req.body);
                
                if (!json) {
                    return crow::response(400, "Invalid JSON");
                }
                
                if (!json.has("name") || !json.has("email")) {
                    return crow::response(400, "Missing required fields: name, email");
                }
                
                std::string name = json["name"].s();
                std::string email = json["email"].s();
                
                bool success = db.updateUser(id, name, email);
                
                if (!success) {
                    return crow::response(404, "User not found");
                }
                
                return crow::response(204);
            });
        
        CROW_ROUTE(app, "/users/<int>")
            .methods(crow::HTTPMethod::DELETE)
            ([&db](const crow::request& req, int id) {
                bool success = db.deleteUser(id);
                
                if (!success) {
                    return crow::response(404, "User not found");
                }
                
                return crow::response(204);
            });
    }
}
