#include "db_handler.h"
#include <iostream>
#include <cstdlib>
#include <sstream>

DBHandler::DBHandler() : conn(nullptr), connected(false) {}

DBHandler::~DBHandler() {
    disconnect();
}

bool DBHandler::connect(const std::string& host, const std::string& port, 
                        const std::string& dbname, const std::string& user, 
                        const std::string& password) {
    std::string conninfo = "host=" + host + " port=" + port + " dbname=" + dbname + 
                          " user=" + user + " password=" + password;
    
    conn = PQconnectdb(conninfo.c_str());
    
    if (PQstatus(conn) != CONNECTION_OK) {
        std::cerr << "Connection to database failed: " << PQerrorMessage(conn) << std::endl;
        PQfinish(conn);
        conn = nullptr;
        connected = false;
        return false;
    }
    
    connected = true;
    return true;
}

void DBHandler::disconnect() {
    if (conn) {
        PQfinish(conn);
        conn = nullptr;
    }
    connected = false;
}

bool DBHandler::isConnected() const {
    return connected && conn && PQstatus(conn) == CONNECTION_OK;
}

std::vector<User> DBHandler::getAllUsers() {
    std::vector<User> users;
    
    if (!isConnected()) {
        return users;
    }
    
    const char* query = "SELECT id, name, email FROM users ORDER BY id";
    PGresult* res = PQexec(conn, query);
    
    if (PQresultStatus(res) != PGRES_TUPLES_OK) {
        std::cerr << "getAllUsers query failed: " << PQerrorMessage(conn) << std::endl;
        PQclear(res);
        return users;
    }
    
    int rows = PQntuples(res);
    for (int i = 0; i < rows; i++) {
        int id = atoi(PQgetvalue(res, i, 0));
        std::string name = PQgetvalue(res, i, 1);
        std::string email = PQgetvalue(res, i, 2);
        
        users.emplace_back(id, name, email);
    }
    
    PQclear(res);
    return users;
}

User DBHandler::getUserById(int id) {
    User user;
    
    if (!isConnected()) {
        return user;
    }
    
    std::stringstream ss;
    ss << "SELECT id, name, email FROM users WHERE id = " << id;
    std::string query = ss.str();
    
    PGresult* res = PQexec(conn, query.c_str());
    
    if (PQresultStatus(res) != PGRES_TUPLES_OK) {
        std::cerr << "getUserById query failed: " << PQerrorMessage(conn) << std::endl;
        PQclear(res);
        return user;
    }
    
    if (PQntuples(res) > 0) {
        user.id = atoi(PQgetvalue(res, 0, 0));
        user.name = PQgetvalue(res, 0, 1);
        user.email = PQgetvalue(res, 0, 2);
    }
    
    PQclear(res);
    return user;
}

User DBHandler::createUser(const std::string& name, const std::string& email) {
    User user;
    
    if (!isConnected()) {
        return user;
    }
    
    std::string query = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email";
    
    const char* paramValues[2];
    paramValues[0] = name.c_str();
    paramValues[1] = email.c_str();
    
    PGresult* res = PQexecParams(conn, query.c_str(), 2, NULL, paramValues, NULL, NULL, 0);
    
    if (PQresultStatus(res) != PGRES_TUPLES_OK) {
        std::cerr << "createUser query failed: " << PQerrorMessage(conn) << std::endl;
        PQclear(res);
        return user;
    }
    
    if (PQntuples(res) > 0) {
        user.id = atoi(PQgetvalue(res, 0, 0));
        user.name = PQgetvalue(res, 0, 1);
        user.email = PQgetvalue(res, 0, 2);
    }
    
    PQclear(res);
    return user;
}

bool DBHandler::updateUser(int id, const std::string& name, const std::string& email) {
    if (!isConnected() || !userExists(id)) {
        return false;
    }
    
    std::string query = "UPDATE users SET name = $1, email = $2 WHERE id = $3";
    
    const char* paramValues[3];
    paramValues[0] = name.c_str();
    paramValues[1] = email.c_str();
    
    std::stringstream ss;
    ss << id;
    std::string idStr = ss.str();
    paramValues[2] = idStr.c_str();
    
    PGresult* res = PQexecParams(conn, query.c_str(), 3, NULL, paramValues, NULL, NULL, 0);
    
    bool success = (PQresultStatus(res) == PGRES_COMMAND_OK);
    
    if (!success) {
        std::cerr << "updateUser query failed: " << PQerrorMessage(conn) << std::endl;
    }
    
    PQclear(res);
    return success;
}

bool DBHandler::deleteUser(int id) {
    if (!isConnected() || !userExists(id)) {
        return false;
    }
    
    std::stringstream ss;
    ss << "DELETE FROM users WHERE id = " << id;
    std::string query = ss.str();
    
    PGresult* res = PQexec(conn, query.c_str());
    
    bool success = (PQresultStatus(res) == PGRES_COMMAND_OK);
    
    if (!success) {
        std::cerr << "deleteUser query failed: " << PQerrorMessage(conn) << std::endl;
    }
    
    PQclear(res);
    return success;
}

bool DBHandler::userExists(int id) {
    if (!isConnected()) {
        return false;
    }
    
    std::stringstream ss;
    ss << "SELECT 1 FROM users WHERE id = " << id;
    std::string query = ss.str();
    
    PGresult* res = PQexec(conn, query.c_str());
    
    if (PQresultStatus(res) != PGRES_TUPLES_OK) {
        std::cerr << "userExists query failed: " << PQerrorMessage(conn) << std::endl;
        PQclear(res);
        return false;
    }
    
    bool exists = (PQntuples(res) > 0);
    PQclear(res);
    
    return exists;
}
