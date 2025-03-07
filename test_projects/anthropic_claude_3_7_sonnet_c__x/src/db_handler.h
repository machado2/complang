#pragma once

#include <string>
#include <vector>
#include <memory>
#include <libpq-fe.h>
#include "models/user.h"

class DBHandler {
private:
    PGconn* conn;
    bool connected;

public:
    DBHandler();
    ~DBHandler();
    
    bool connect(const std::string& host, const std::string& port, 
                 const std::string& dbname, const std::string& user, 
                 const std::string& password);
    void disconnect();
    bool isConnected() const;
    
    // User CRUD operations
    std::vector<User> getAllUsers();
    User getUserById(int id);
    User createUser(const std::string& name, const std::string& email);
    bool updateUser(int id, const std::string& name, const std::string& email);
    bool deleteUser(int id);
    
private:
    // Helper method to check if a user exists
    bool userExists(int id);
};
