#include "crow_all.h"
#include <pqxx/pqxx>
#include <sstream>
#include <cstdlib>
#include <iostream>

// Helper: Build PostgreSQL connection string using PGPASSWORD env variable.
std::string get_db_connection_string() {
    const char* pgpwd = std::getenv("PGPASSWORD");
    std::string pwd = pgpwd ? pgpwd : "";
    std::ostringstream conn;
    conn << "host=host.docker.internal port=5432 dbname=complang user=testuser password=" << pwd;
    return conn.str();
}

int main()
{
    crow::SimpleApp app;

    // POST /users: Create a new user.
    CROW_ROUTE(app, "/users").methods("POST"_method)([](const crow::request& req){
        auto body = crow::json::load(req.body);
        if (!body) {
            return crow::response(400, "Invalid JSON");
        }
        if (!body.has("name") || !body.has("email")) {
            return crow::response(400, "Missing name or email");
        }
        std::string name = body["name"].s();
        std::string email = body["email"].s();
        std::string conn_str = get_db_connection_string();
        try {
            pqxx::connection conn(conn_str);
            pqxx::work txn(conn);
            std::ostringstream query;
            query << "INSERT INTO users(name, email) VALUES(" 
                  << txn.quote(name) << ", " 
                  << txn.quote(email) 
                  << ") RETURNING id;";
            pqxx::result r = txn.exec(query.str());
            txn.commit();
            if (r.size() > 0) {
                int id = r[0][0].as<int>();
                crow::json::wvalue resp_body;
                resp_body["id"] = id;
                resp_body["name"] = name;
                resp_body["email"] = email;
                crow::response resp(201);
                resp.write(resp_body.dump());
                return resp;
            }
            return crow::response(500, "Failed to create user");
        } catch (const std::exception &e) {
            return crow::response(500, e.what());
        }
    });

    // GET /users: Retrieve all users.
    CROW_ROUTE(app, "/users").methods("GET"_method)([](){
        std::string conn_str = get_db_connection_string();
        try {
            pqxx::connection conn(conn_str);
            pqxx::work txn(conn);
            pqxx::result r = txn.exec("SELECT id, name, email FROM users;");
            crow::json::wvalue resp_body = crow::json::wvalue::list();
            for (const auto &row : r) {
                crow::json::wvalue user;
                user["id"] = row[0].as<int>();
                user["name"] = row[1].c_str();
                user["email"] = row[2].c_str();
                resp_body.push_back(user);
            }
            return crow::response(200, resp_body.dump());
        } catch (const std::exception &e) {
            return crow::response(500, e.what());
        }
    });

    // GET /users/{id}: Retrieve a single user.
    CROW_ROUTE(app, "/users/<int>").methods("GET"_method)([](int id){
        std::string conn_str = get_db_connection_string();
        try {
            pqxx::connection conn(conn_str);
            pqxx::work txn(conn);
            std::ostringstream query;
            query << "SELECT id, name, email FROM users WHERE id = " << id << ";";
            pqxx::result r = txn.exec(query.str());
            if (r.size() == 0) {
                return crow::response(404, "User not found");
            }
            crow::json::wvalue user;
            user["id"] = r[0][0].as<int>();
            user["name"] = r[0][1].c_str();
            user["email"] = r[0][2].c_str();
            return crow::response(200, user.dump());
        } catch (const std::exception &e) {
            return crow::response(500, e.what());
        }
    });

    // PUT /users/{id}: Update an existing user.
    CROW_ROUTE(app, "/users/<int>").methods("PUT"_method)([](const crow::request& req, int id){
        auto body = crow::json::load(req.body);
        if (!body) {
            return crow::response(400, "Invalid JSON");
        }
        if (!body.has("name") || !body.has("email")) {
            return crow::response(400, "Missing name or email");
        }
        std::string name = body["name"].s();
        std::string email = body["email"].s();
        std::string conn_str = get_db_connection_string();
        try {
            pqxx::connection conn(conn_str);
            pqxx::work txn(conn);
            std::ostringstream query;
            query << "UPDATE users SET name = " << txn.quote(name)
                  << ", email = " << txn.quote(email)
                  << " WHERE id = " << id << " RETURNING id;";
            pqxx::result r = txn.exec(query.str());
            txn.commit();
            if (r.size() == 0) {
                return crow::response(404, "User not found");
            }
            crow::json::wvalue user;
            user["id"] = id;
            user["name"] = name;
            user["email"] = email;
            return crow::response(200, user.dump());
        } catch (const std::exception &e) {
            return crow::response(500, e.what());
        }
    });

    // DELETE /users/{id}: Delete a user.
    CROW_ROUTE(app, "/users/<int>").methods("DELETE"_method)([](int id){
        std::string conn_str = get_db_connection_string();
        try {
            pqxx::connection conn(conn_str);
            pqxx::work txn(conn);
            std::ostringstream query;
            query << "DELETE FROM users WHERE id = " << id << " RETURNING id;";
            pqxx::result r = txn.exec(query.str());
            txn.commit();
            if (r.size() == 0) {
                return crow::response(404, "User not found");
            }
            return crow::response(204);
        } catch (const std::exception &e) {
            return crow::response(500, e.what());
        }
    });

    app.port(8080).multithreaded().run();
    return 0;
}
