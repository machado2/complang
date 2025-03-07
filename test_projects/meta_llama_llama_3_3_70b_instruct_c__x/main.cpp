
#include "crow.h"
#include <pqxx/pqxx>
#include <iostream>

int main() {
    // Load the password from the environment variable
    const char* password = std::getenv("PGPASSWORD");

    // Create a connection to the PostgreSQL database
    pqxx::connection conn(" dbname=complang user=testuser password=" + std::string(password) + " host=host.docker.internal port=5432");

    // Create a Crow application
    crow::SimpleApp app;

    // Define the endpoints
    CROW_ROUTE(app, "/users")(
        [](const crow::request& req) {
            if (req.method == crow::HTTPMethod::POST) {
                // Create a new user
                auto x = crow::json::load(req.body);
                pqxx::work w(conn);
                w.exec("INSERT INTO users (name, email) VALUES ('" + x["name"].s + "', '" + x["email"].s + "') RETURNING id, name, email");
                pqxx::result r = w.exec("SELECT id, name, email FROM users WHERE id = (SELECT MAX(id) FROM users)");
                w.commit();
                return crow::response(201, crow::json{{"id", r[0][0].as<int>()}, {"name", r[0][1].as<std::string>()}, {"email", r[0][2].as<std::string>()}});
            } else if (req.method == crow::HTTPMethod::GET) {
                // Get all users
                pqxx::work w(conn);
                pqxx::result r = w.exec("SELECT id, name, email FROM users");
                crow::json users;
                for (auto row : r) {
                    users.push_back(crow::json{{"id", row["id"].as<int>()}, {"name", row["name"].as<std::string>()}, {"email", row["email"].as<std::string>()}});
                }
                return crow::response(200, users);
            }
            return crow::response(405);
        }
    );

    CROW_ROUTE(app, "/users/<int>")(
        [](int id) {
            pqxx::work w(conn);
            pqxx::result r = w.exec("SELECT id, name, email FROM users WHERE id = " + std::to_string(id));
            if (r.size() == 0) {
                return crow::response(404);
            }
            return crow::response(200, crow::json{{"id", r[0][0].as<int>()}, {"name", r[0][1].as<std::string>()}, {"email", r[0][2].as<std::string>()}});
        }
    );

    CROW_ROUTE(app, "/users/<int>").methods(crow::HTTPMethod::PUT)(
        [](int id, const crow::request& req) {
            auto x = crow::json::load(req.body);
            pqxx::work w(conn);
            w.exec("UPDATE users SET name = '" + x["name"].s + "', email = '" + x["email"].s + "' WHERE id = " + std::to_string(id));
            w.commit();
            return crow::response(200);
        }
    );

    CROW_ROUTE(app, "/users/<int>").methods(crow::HTTPMethod::DELETE)(
        [](int id) {
            pqxx::work w(conn);
            w.exec("DELETE FROM users WHERE id = " + std::to_string(id));
            w.commit();
            return crow::response(200);
        }
    );

    app.port(8080).run();
}
