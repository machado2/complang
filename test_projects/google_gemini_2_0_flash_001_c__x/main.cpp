
#include <iostream>
#include <string>
#include <vector>
#include <sstream>

#include <cpprest/http.h>
#include <cpprest/http_listener.h>
#include <cpprest/json.h>

#include <pqxx/pqxx>

using namespace web;
using namespace web::http;
using namespace web::http::client;
using namespace web::http::listeners;

using json = web::json::value;

// Database credentials
const std::string db_host = "host.docker.internal";
const int db_port = 5432;
const std::string db_name = "complang";
const std::string db_user = "testuser";

// Function to retrieve the password from the environment variable
std::string get_password() {
    const char* password = std::getenv("PGPASSWORD");
    if (password == nullptr) {
        throw std::runtime_error("PGPASSWORD environment variable not set.");
    }
    return std::string(password);
}

// Structure representing a user
struct User {
    int id;
    std::string name;
    std::string email;
};

// Helper function to convert a User struct to a JSON value
json user_to_json(const User& user) {
    json j;
    j["id"] = user.id;
    j["name"] = user.name;
    j["email"] = user.email;
    return j;
}

// Helper function to convert a JSON value to a User struct
User json_to_user(const json& j) {
    User user;
    user.id = j.has_field("id") ? j.at("id").as_integer() : -1;
    user.name = j.at("name").as_string();
    user.email = j.at("email").as_string();
    return user;
}

// Function to handle GET requests for all users
void handle_get_all_users(http_request request) {
    std::vector<User> users;
    try {
        pqxx::connection c("host=" + db_host + " port=" + std::to_string(db_port) + " dbname=" + db_name + " user=" + db_user + " password=" + get_password());
        pqxx::work txn(c);
        pqxx::result r = txn.exec("SELECT id, name, email FROM users");
        for (auto row : r) {
            User user;
            user.id = row["id"].as<int>();
            user.name = row["name"].as<std::string>();
            user.email = row["email"].as<std::string>();
            users.push_back(user);
        }
        txn.commit();
        c.disconnect();

        json response = json::array();
        for (const auto& user : users) {
            response.push_back(user_to_json(user));
        }
        request.reply(status_codes::OK, response);

    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        request.reply(status_codes::InternalError, json::string(e.what()));
    }
}

// Function to handle GET requests for a single user by ID
void handle_get_user_by_id(http_request request, const std::string& id_str) {
    try {
        int id = std::stoi(id_str);
        pqxx::connection c("host=" + db_host + " port=" + std::to_string(db_port) + " dbname=" + db_name + " user=" + db_user + " password=" + get_password());
        pqxx::work txn(c);
        pqxx::result r = txn.exec_params("SELECT id, name, email FROM users WHERE id = $1", id);
        if (r.empty()) {
            request.reply(status_codes::NotFound);
        } else {
            auto row = r.front();
            User user;
            user.id = row["id"].as<int>();
            user.name = row["name"].as<std::string>();
            user.email = row["email"].as<std::string>();
            request.reply(status_codes::OK, user_to_json(user));
        }
        txn.commit();
        c.disconnect();
    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        request.reply(status_codes::InternalError, json::string(e.what()));
    }
}

// Function to handle POST requests to create a new user
void handle_post_user(http_request request) {
    request.extract_json().then([&](json::value body) {
        try {
            User user = json_to_user(body);

            pqxx::connection c("host=" + db_host + " port=" + std::to_string(db_port) + " dbname=" + db_name + " user=" + db_user + " password=" + get_password());
            pqxx::work txn(c);
            pqxx::result r = txn.exec_params("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id", user.name, user.email);
            int new_id = r.front()["id"].as<int>();
            user.id = new_id;

            txn.commit();
            c.disconnect();

            request.reply(status_codes::Created, user_to_json(user));
        } catch (const std::exception& e) {
            std::cerr << e.what() << std::endl;
            request.reply(status_codes::InternalError, json::string(e.what()));
        }
    }).wait();
}

// Function to handle PUT requests to update an existing user
void handle_put_user(http_request request, const std::string& id_str) {
    request.extract_json().then([&](json::value body) {
        try {
            int id = std::stoi(id_str);
            User user = json_to_user(body);
            user.id = id;

            pqxx::connection c("host=" + db_host + " port=" + std::to_string(db_port) + " dbname=" + db_name + " user=" + db_user + " password=" + get_password());
            pqxx::work txn(c);
            pqxx::result r = txn.exec_params("UPDATE users SET name = $1, email = $2 WHERE id = $3", user.name, user.email, id);
            if (r.affected_rows() == 0) {
                request.reply(status_codes::NotFound);
            } else {
                txn.commit();
                c.disconnect();
                request.reply(status_codes::NoContent);
            }


        } catch (const std::exception& e) {
            std::cerr << e.what() << std::endl;
            request.reply(status_codes::InternalError, json::string(e.what()));
        }
    }).wait();
}

// Function to handle DELETE requests to delete a user
void handle_delete_user(http_request request, const std::string& id_str) {
    try {
        int id = std::stoi(id_str);

        pqxx::connection c("host=" + db_host + " port=" + std::to_string(db_port) + " dbname=" + db_name + " user=" + db_user + " password=" + get_password());
        pqxx::work txn(c);
        pqxx::result r = txn.exec_params("DELETE FROM users WHERE id = $1", id);
        if (r.affected_rows() == 0) {
            request.reply(status_codes::NotFound);
        } else {
             txn.commit();
             c.disconnect();
             request.reply(status_codes::NoContent);
        }
    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        request.reply(status_codes::InternalError, json::string(e.what()));
    }
}


int main() {
    http_listener listener("http://0.0.0.0:8080/users");

    listener.support(methods::GET, [&](http_request request) {
        auto path = request.relative_uri().path();
        if (path == "/") {
            handle_get_all_users(request);
        } else {
            auto id = path.substr(1);
            handle_get_user_by_id(request, id);
        }
    });

    listener.support(methods::POST, handle_post_user);

     listener.support(methods::PUT, [&](http_request request) {
        auto path = request.relative_uri().path();
        auto id = path.substr(1);
        handle_put_user(request, id);
    });

    listener.support(methods::DELETE, [&](http_request request) {
        auto path = request.relative_uri().path();
        auto id = path.substr(1);
        handle_delete_user(request, id);
    });

    try {
        listener
            .open()
            .then([&]() { std::cout << "Starting to listen" << std::endl; })
            .wait();

        while (true)
            ;
    } catch (const std::exception& e) {
        std::cerr << "ERROR: " << e.what() << std::endl;
    }

    return 0;
}
