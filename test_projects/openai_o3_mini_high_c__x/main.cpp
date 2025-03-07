#include "crow_all.h"
#include <pqxx/pqxx>
#include <cstdlib>
#include <string>
#include <exception>

int main() {
    crow::SimpleApp app;

    // POST /users: Create a new user
    CROW_ROUTE(app, "/users").methods(crow::HTTPMethod::POST)([](const crow::request& req) -> crow::response {
        try {
            auto body = crow::json::load(req.body);
            if (!body)
                return crow::response(400, "Invalid JSON");
            if (!body.has("name") || !body.has("email"))
                return crow::response(400, "Missing name or email");

            std::string name = body["name"].s();
            std::string email = body["email"].s();

            const char* pg_pass = std::getenv("PGPASSWORD");
            if (!pg_pass)
                return crow::response(500, "Missing PGPASSWORD env variable");

            std::string conn_str = "host=host.docker.internal port=5432 dbname=complang user=testuser password=";
            conn_str += pg_pass;
            pqxx::connection conn(conn_str);
            if (!conn.is_open())
                return crow::response(500, "Could not open DB connection");

            pqxx::work txn(conn);
            std::string query = "INSERT INTO users (name, email) VALUES (" + txn.quote(name) + ", " + txn.quote(email) + ") RETURNING id";
            pqxx::result res = txn.exec(query);
            txn.commit();

            if (res.size() != 1)
                return crow::response(500, "Insertion failed");

            int id = res[0][0].as<int>();

            crow::json::wvalue ret;
            ret["id"] = id;
            ret["name"] = name;
            ret["email"] = email;
            crow::response response(ret);
            response.code = 201;
            return response;
        } catch (std::exception &e) {
            return crow::response(500, std::string("Exception: ") + e.what());
        }
    });

    // GET /users: Retrieve all users
    CROW_ROUTE(app, "/users").methods(crow::HTTPMethod::GET)([]() -> crow::response {
        try {
            const char* pg_pass = std::getenv("PGPASSWORD");
            if (!pg_pass)
                return crow::response(500, "Missing PGPASSWORD env variable");
            std::string conn_str = "host=host.docker.internal port=5432 dbname=complang user=testuser password=";
            conn_str += pg_pass;
            pqxx::connection conn(conn_str);
            if (!conn.is_open())
                return crow::response(500, "Could not open DB connection");
            pqxx::work txn(conn);
            pqxx::result res = txn.exec("SELECT id, name, email FROM users ORDER BY id");
            txn.commit();

            crow::json::wvalue ret;
            for (size_t i = 0; i < res.size(); ++i) {
                crow::json::wvalue user;
                user["id"] = res[i][0].as<int>();
                user["name"] = res[i][1].as<std::string>();
                user["email"] = res[i][2].as<std::string>();
                ret[i] = user;
            }
            return crow::response(ret);
        } catch (std::exception &e) {
            return crow::response(500, std::string("Exception: ") + e.what());
        }
    });

    // GET /users/<id>: Retrieve a user by id
    CROW_ROUTE(app, "/users/<int>").methods(crow::HTTPMethod::GET)([](int id) -> crow::response {
        try {
            const char* pg_pass = std::getenv("PGPASSWORD");
            if (!pg_pass)
                return crow::response(500, "Missing PGPASSWORD env variable");
            std::string conn_str = "host=host.docker.internal port=5432 dbname=complang user=testuser password=";
            conn_str += pg_pass;
            pqxx::connection conn(conn_str);
            if (!conn.is_open())
                return crow::response(500, "Could not open DB connection");
            pqxx::work txn(conn);
            std::string query = "SELECT id, name, email FROM users WHERE id = " + txn.quote(id);
            pqxx::result res = txn.exec(query);
            txn.commit();
            if (res.empty())
                return crow::response(404, "User not found");

            crow::json::wvalue ret;
            ret["id"] = res[0][0].as<int>();
            ret["name"] = res[0][1].as<std::string>();
            ret["email"] = res[0][2].as<std::string>();
            return crow::response(ret);
        } catch (std::exception &e) {
            return crow::response(500, std::string("Exception: ") + e.what());
        }
    });

    // PUT /users/<id>: Update a user
    CROW_ROUTE(app, "/users/<int>").methods(crow::HTTPMethod::PUT)([](int id, const crow::request &req) -> crow::response {
        try {
            auto body = crow::json::load(req.body);
            if (!body)
                return crow::response(400, "Invalid JSON");
            if (!body.has("name") || !body.has("email"))
                return crow::response(400, "Missing name or email");

            std::string name = body["name"].s();
            std::string email = body["email"].s();
            const char* pg_pass = std::getenv("PGPASSWORD");
            if (!pg_pass)
                return crow::response(500, "Missing PGPASSWORD env variable");
            std::string conn_str = "host=host.docker.internal port=5432 dbname=complang user=testuser password=";
            conn_str += pg_pass;
            pqxx::connection conn(conn_str);
            if (!conn.is_open())
                return crow::response(500, "Could not open DB connection");
            pqxx::work txn(conn);
            std::string query = "UPDATE users SET name = " + txn.quote(name) + ", email = " + txn.quote(email) + " WHERE id = " + txn.quote(id) + " RETURNING id";
            pqxx::result res = txn.exec(query);
            if (res.empty()) {
                txn.abort();
                return crow::response(404, "User not found");
            }
            txn.commit();
            return crow::response(200);
        } catch (std::exception &e) {
            return crow::response(500, std::string("Exception: ") + e.what());
        }
    });

    // DELETE /users/<id>: Delete a user
    CROW_ROUTE(app, "/users/<int>").methods(crow::HTTPMethod::DELETE)([](int id) -> crow::response {
        try {
            const char* pg_pass = std::getenv("PGPASSWORD");
            if (!pg_pass)
                return crow::response(500, "Missing PGPASSWORD env variable");
            std::string conn_str = "host=host.docker.internal port=5432 dbname=complang user=testuser password=";
            conn_str += pg_pass;
            pqxx::connection conn(conn_str);
            if (!conn.is_open())
                return crow::response(500, "Could not open DB connection");
            pqxx::work txn(conn);
            std::string query = "DELETE FROM users WHERE id = " + txn.quote(id) + " RETURNING id";
            pqxx::result res = txn.exec(query);
            if (res.empty()) {
                txn.abort();
                return crow::response(404, "User not found");
            }
            txn.commit();
            return crow::response(200);
        } catch (std::exception &e) {
            return crow::response(500, std::string("Exception: ") + e.what());
        }
    });

    app.port(8080).multithreaded().run();
    return 0;
}