#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <stdexcept>
#include <pqxx/pqxx>
#include <nlohmann/json.hpp>

// using namespace Pistache;
using json = nlohmann::json;
using namespace std;

// Helper function to convert pqxx::result to JSON
json pqxxResultToJson(const pqxx::result& res) {
    json j = json::array();
    for (const auto& row : res) {
        json row_json;
        row_json["id"] = row["id"].as<int>();
        row_json["name"] = row["name"].as<string>();
        row_json["email"] = row["email"].as<string>();
        j.push_back(row_json);
    }
    return j;
}

int main() {
    std::cout << "Starting the Complang C++ CRUD API..." << std::endl;

    try {
        // Database connection parameters
        const char* host = "host.docker.internal";
        const int port = 5432;
        const char* dbname = "complang";
        const char* user = "testuser";
        const char* password = std::getenv("PGPASSWORD");

        if (password == nullptr) {
            std::cerr << "Error: PGPASSWORD environment variable not set." << std::endl;
            return 1;
        }

        // Construct connection string
        std::string connection_string = "host=" + std::string(host) + " port=" + std::to_string(port) +
                                        " dbname=" + std::string(dbname) + " user=" + std::string(user) +
                                        " password=" + std::string(password);

        // Connect to the database
        pqxx::connection c(connection_string.c_str());
        if (c.is_open()) {
            std::cout << "Connected to " << c.dbname() << std::endl;
        } else {
            std::cerr << "Failed to connect to the database." << std::endl;
            return 1;
        }

        //***  API logic ***
        // 1. POST /users: Creates a user
        // Parse JSON from request body
        // Extract name and email
        // Insert into the users table
        // Return 201 with { id, name, email }

        pqxx::work txn(c);

        // Insert a user
        string name = "test_name";
        string email = "test@email.com";
        string sql_insert = "INSERT INTO users (name, email) VALUES ('" + name + "', '" + email + "') RETURNING id, name, email;";

        pqxx::result r = txn.exec(sql_insert);

        json user_json = json::object();
        if (!r.empty()) {
             user_json["id"] = r[0]["id"].as<int>();
             user_json["name"] = r[0]["name"].as<string>();
             user_json["email"] = r[0]["email"].as<string>();
        }

        cout << "Inserted user: " << user_json.dump() << endl;
        

        // 2. GET /users: Returns a list of all users
        string sql_select_all = "SELECT id, name, email FROM users;";

        pqxx::result res = txn.exec(sql_select_all);

        json users_json = pqxxResultToJson(res);

        std::cout << "All users:" << users_json.dump() << std::endl;

        txn.commit();
        c.disconnect();
        std::cout << "Disconnected from the database." << std::endl;

    } catch (const std::exception &e) {
        std::cerr << "Exception: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}
