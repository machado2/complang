
#include <iostream>
#include <string>
#include <sstream>
#include <vector>

#include <cpprest/http.h>
#include <cpprest/http_listener.h>
#include <cpprest/json.h>

#include <pqxx/pqxx>

#include "db.h"
#include "user.h"

using namespace std;
using namespace web;
using namespace web::http;
using namespace web::http::client;
using namespace web::http::listeners;

// PostgreSQL configuration
const string DB_HOST = "host.docker.internal";
const int DB_PORT = 5432;
const string DB_NAME = "complang";
const string DB_USER = "testuser";

int main() {
    // Get password from environment variable
    const char* pgpassword = std::getenv("PGPASSWORD");
    if (pgpassword == nullptr) {
        cerr << "Error: PGPASSWORD environment variable not set." << endl;
        return 1;
    }
    string DB_PASSWORD = pgpassword;

    // Initialize database connection pool
    DB db(DB_HOST, DB_PORT, DB_NAME, DB_USER, DB_PASSWORD);
    if (!db.is_open()) {
        cerr << "Failed to connect to the database." << endl;
        return 1;
    }

    http_listener listener("http://0.0.0.0:8080");

    listener.support(methods::GET, [&db](http_request request) {
        uri uri = request.relative_uri();
        string path = uri.path();
        
        if (path == "/users") {
            // Get all users
            vector<User> users = db.get_all_users();
            json::value response = json::value::array();
            int i = 0;
            for (const auto& user : users) {
                json::value user_json;
                user_json["id"] = json::value::number(user.id);
                user_json["name"] = json::value::string(user.name);
                user_json["email"] = json::value::string(user.email);
                response[i++] = user_json;
            }
            request.reply(status_codes::OK, response);
        } else if (path.find("/users/") == 0) {
            // Get user by id
            try {
                int id = stoi(path.substr(7));
                User user = db.get_user(id);
                if (user.id != -1) {
                    json::value response;
                    response["id"] = json::value::number(user.id);
                    response["name"] = json::value::string(user.name);
                    response["email"] = json::value::string(user.email);
                    request.reply(status_codes::OK, response);
                } else {
                    request.reply(status_codes::NotFound);
                }
            } catch (const invalid_argument& e) {
                request.reply(status_codes::BadRequest);
            }
        } else {
            request.reply(status_codes::NotFound);
        }
    });

    listener.support(methods::POST, [&db](http_request request) {
        uri uri = request.relative_uri();
        string path = uri.path();

        if (path == "/users") {
            request.extract_json().then([&db, &request](json::value json_value) {
                try {
                    string name = json_value.at("name").as_string();
                    string email = json_value.at("email").as_string();

                    User new_user = db.create_user(name, email);
                    json::value response;
                    response["id"] = json::value::number(new_user.id);
                    response["name"] = json::value::string(new_user.name);
                    response["email"] = json::value::string(new_user.email);
                    request.reply(status_codes::Created, response);
                } catch (const json::json_exception& e) {
                    request.reply(status_codes::BadRequest);
                }
            }).wait();
        } else {
            request.reply(status_codes::NotFound);
        }
    });

    listener.support(methods::PUT, [&db](http_request request) {
        uri uri = request.relative_uri();
        string path = uri.path();

        if (path.find("/users/") == 0) {
            try {
                int id = stoi(path.substr(7));
                request.extract_json().then([&db, id, &request](json::value json_value) {
                    try {
                        string name = json_value.at("name").as_string();
                        string email = json_value.at("email").as_string();

                        bool updated = db.update_user(id, name, email);
                        if (updated) {
                            request.reply(status_codes::NoContent);
                        } else {
                            request.reply(status_codes::NotFound);
                        }
                    } catch (const json::json_exception& e) {
                        request.reply(status_codes::BadRequest);
                    }
                }).wait();
            } catch (const invalid_argument& e) {
                 request.reply(status_codes::BadRequest);
            }
        } else {
            request.reply(status_codes::NotFound);
        }
    });

    listener.support(methods::DEL, [&db](http_request request) {
        uri uri = request.relative_uri();
        string path = uri.path();
    
        if (path.find("/users/") == 0) {
            try {
                int id = stoi(path.substr(7));
                bool deleted = db.delete_user(id);
                if (deleted) {
                    request.reply(status_codes::NoContent);
                } else {
                    request.reply(status_codes::NotFound);
                }
            } catch (const invalid_argument& e) {
                request.reply(status_codes::BadRequest);
            }
        } else {
            request.reply(status_codes::NotFound);
        }
    });

    try {
        listener
            .open()
            .then([&listener]() { cout << "Starting to listen on: " << listener.uri().to_string() << endl; })
            .wait();

        getchar();
    } catch (exception const& e) {
        cout << "ERROR: " << e.what() << endl;
    }

    return 0;
}
