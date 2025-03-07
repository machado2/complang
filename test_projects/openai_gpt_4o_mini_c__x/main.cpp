#include <crow.h>
#include <pqxx/pqxx>
#include <nlohmann/json.hpp>
#include <iostream>
#include <string>

using json = nlohmann::json;

class UserAPI {
public:
    UserAPI(const std::string& connection_string) : db_connection(connection_string) {
        db_connection.prepare("insert_user", "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email");
        db_connection.prepare("get_users", "SELECT id, name, email FROM users");
        db_connection.prepare("get_user", "SELECT id, name, email FROM users WHERE id = $1");
        db_connection.prepare("update_user", "UPDATE users SET name = $1, email = $2 WHERE id = $3");
        db_connection.prepare("delete_user", "DELETE FROM users WHERE id = $1");
    }

    void create_user(const crow::request& req, crow::response& res) {
        auto body = json::parse(req.body);
        std::string name = body["name"];
        std::string email = body["email"];
        pqxx::work txn(db_connection);
        pqxx::result r = txn.prepared("insert_user")(name)(email).exec();
        txn.commit();
        
        json response = {{"id", r[0][0].as<int>()}, {"name", name}, {"email", email}};
        res.code = 201;
        res.body = response.dump();
        res.set_header("Content-Type", "application/json");
        res.end();
    }

    void get_users(const crow::request& req, crow::response& res) {
        pqxx::work txn(db_connection);
        pqxx::result r = txn.prepared("get_users").exec();
        json response = json::array();
        
        for (auto row : r) {
            response.push_back({{"id", row[0].as<int>()}, {"name", row[1].as<std::string>()}, {"email", row[2].as<std::string>()}});
        }
        
        res.code = 200;
        res.body = response.dump();
        res.set_header("Content-Type", "application/json");
        res.end();
    }

    void get_user(const crow::request& req, crow::response& res, int id) {
        pqxx::work txn(db_connection);
        pqxx::result r = txn.prepared("get_user")(id).exec();
        
        if (r.empty()) {
            res.code = 404;
            res.body = "User not found";
            res.end();
            return;
        }

        json response = {{"id", r[0][0].as<int>()}, {"name", r[0][1].as<std::string>()}, {"email", r[0][2].as<std::string>()}};
        res.code = 200;
        res.body = response.dump();
        res.set_header("Content-Type", "application/json");
        res.end();
    }

    void update_user(const crow::request& req, crow::response& res, int id) {
        auto body = json::parse(req.body);
        std::string name = body["name"];
        std::string email = body["email"];
        pqxx::work txn(db_connection);
        size_t updated = txn.prepared("update_user")(name)(email)(id).exec();
        txn.commit();
        
        if (updated == 0) {
            res.code = 404;
            res.body = "User not found";
        } else {
            res.code = 204;
        }
        res.end();
    }

    void delete_user(const crow::request& req, crow::response& res, int id) {
        pqxx::work txn(db_connection);
        size_t deleted = txn.prepared("delete_user")(id).exec();
        txn.commit();
        
        if (deleted == 0) {
            res.code = 404;
            res.body = "User not found";
        } else {
            res.code = 204;
        }
        res.end();
    }

private:
    pqxx::connection db_connection;
};

int main() {
    crow::SimpleApp app;
    std::string db_connection_str = "dbname=complang user=testuser password=" + std::string(getenv("PGPASSWORD")) + " host=host.docker.internal port=5432";

    UserAPI api(db_connection_str);

    CROW_ROUTE(app, "/users").methods("POST"_method)([&api](const crow::request& req, crow::response& res) {
        api.create_user(req, res);
    });

    CROW_ROUTE(app, "/users")([&api](const crow::request& req, crow::response& res) {
        api.get_users(req, res);
    });

    CROW_ROUTE(app, "/users/<int>")([&api](const crow::request& req, crow::response& res, int id) {
        api.get_user(req, res, id);
    });

    CROW_ROUTE(app, "/users/<int>").methods("PUT"_method)([&api](const crow::request& req, crow::response& res, int id) {
        api.update_user(req, res, id);
    });

    CROW_ROUTE(app, "/users/<int>").methods("DELETE"_method)([&api](const crow::request& req, crow::response& res, int id) {
        api.delete_user(req, res, id);
    });

    app.port(8080).multithreaded().run();
}