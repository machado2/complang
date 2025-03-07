#pragma once

#include <string>

struct User {
    int id;
    std::string name;
    std::string email;
    
    // Constructor
    User(int id = 0, std::string name = "", std::string email = "")
        : id(id), name(std::move(name)), email(std::move(email)) {}
};
