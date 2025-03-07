#include "user.h"
#include "db.h"
#include <pqxx/pqxx>
#include <iostream>

std::vector<User> get_all_users() {
    pqxx::connection conn = get_db_connection();
    pqxx::work txn(conn);
    pqxx::result res = txn.exec("SELECT id, name, email FROM users");
    std::vector<User> users;
    for (auto row : res) {
        User user;
        user.id = row["id"].as<int>();
        user.name = row["name"].as<std::string>();
        user.email = row["email"].as<std::string>();
        users.push_back(user);
    }
    txn.commit();
    return users;
}

std::optional<User> get_user_by_id(int id) {
    pqxx::connection conn = get_db_connection();
    pqxx::work txn(conn);
    pqxx::result res = txn.exec_params("SELECT id, name, email FROM users WHERE id = $1", id);
    if (res.empty()) {
        return std::nullopt;
    }
    auto row = res.front();
    User user;
    user.id = row["id"].as<int>();
    user.name = row["name"].as<std::string>();
    user.email = row["email"].as<std::string>();
    txn.commit();
    return user;
}

User create_user(std::string name, std::string email) {
    pqxx::connection conn = get_db_connection();
    pqxx::work txn(conn);
    pqxx::result res = txn.exec_params("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email", name, email);
    auto row = res.front();
    User user;
    user.id = row["id"].as<int>();
    user.name = row["name"].as<std::string>();
    user.email = row["email"].as<std::string>();
    txn.commit();
    return user;
}

bool update_user(int id, std::string name, std::string email) {
    pqxx::connection conn = get_db_connection();
    pqxx::work txn(conn);
    pqxx::result res = txn.exec_params("UPDATE users SET name = $1, email = $2 WHERE id = $3", name, email, id);
    txn.commit();
    return res.affected_rows() > 0;
}

bool delete_user(int id) {
    pqxx::connection conn = get_db_connection();
    pqxx::work txn(conn);
    pqxx::result res = txn.exec_params("DELETE FROM users WHERE id = $1", id);
    txn.commit();
    return res.affected_rows() > 0;
}