
#include "db.h"

#include <iostream>
#include <pqxx/pqxx>
#include <vector>

using namespace std;

DB::DB(const string& host, int port, const string& dbname, const string& user, const string& password)
    : host(host), port(port), dbname(dbname), user(user), password(password) {
    try {
        string connection_string = "host=" + host + " port=" + to_string(port) + " dbname=" + dbname +
                                     " user=" + user + " password=" + password;
        conn = make_unique<pqxx::connection>(connection_string);
        if (conn->is_open()) {
            cout << "Connected to database" << endl;
        } else {
            cerr << "Failed to connect to database" << endl;
        }
    } catch (const exception& e) {
        cerr << "Exception in DB constructor: " << e.what() << endl;
    }
}

DB::~DB() {
    if (conn && conn->is_open()) {
        conn->close();
        cout << "Database connection closed" << endl;
    }
}

bool DB::is_open() const {
    return conn && conn->is_open();
}

User DB::create_user(const string& name, const string& email) {
    try {
        pqxx::work txn(*conn);
        pqxx::result res = txn.exec_params(
            "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email",
            name, email);
        txn.commit();

        if (!res.empty()) {
            int id = res[0]["id"].as<int>();
            string name = res[0]["name"].as<string>();
            string email = res[0]["email"].as<string>();
            return User(id, name, email);
        } else {
            return User(-1, "", ""); // Indicate failure
        }
    } catch (const exception& e) {
        cerr << "Exception in create_user: " << e.what() << endl;
        return User(-1, "", ""); // Indicate failure
    }
}

User DB::get_user(int id) {
    try {
        pqxx::work txn(*conn);
        pqxx::result res = txn.exec_params(
            "SELECT id, name, email FROM users WHERE id = $1", id);
        txn.commit();

        if (!res.empty()) {
            int id = res[0]["id"].as<int>();
            string name = res[0]["name"].as<string>();
            string email = res[0]["email"].as<string>();
            return User(id, name, email);
        } else {
            return User(-1, "", ""); // Indicate not found
        }
    } catch (const exception& e) {
        cerr << "Exception in get_user: " << e.what() << endl;
        return User(-1, "", ""); // Indicate not found
    }
}

vector<User> DB::get_all_users() {
    vector<User> users;
    try {
        pqxx::work txn(*conn);
        pqxx::result res = txn.exec("SELECT id, name, email FROM users");
        txn.commit();

        for (const auto& row : res) {
            int id = row["id"].as<int>();
            string name = row["name"].as<string>();
            string email = row["email"].as<string>();
            users.emplace_back(id, name, email);
        }
    } catch (const exception& e) {
        cerr << "Exception in get_all_users: " << e.what() << endl;
    }
    return users;
}

bool DB::update_user(int id, const string& name, const string& email) {
    try {
        pqxx::work txn(*conn);
        pqxx::result res = txn.exec_params(
            "UPDATE users SET name = $1, email = $2 WHERE id = $3",
            name, email, id);
        txn.commit();

        return res.affected_rows() > 0;
    } catch (const exception& e) {
        cerr << "Exception in update_user: " << e.what() << endl;
        return false;
    }
}

bool DB::delete_user(int id) {
    try {
        pqxx::work txn(*conn);
        pqxx::result res = txn.exec_params(
            "DELETE FROM users WHERE id = $1", id);
        txn.commit();

        return res.affected_rows() > 0;
    } catch (const exception& e) {
        cerr << "Exception in delete_user: " << e.what() << endl;
        return false;
    }
}
