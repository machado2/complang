#ifndef USER_H
#define USER_H

#include <string>
#include <optional>

struct User {
    int id;
    std::string name;
    std::string email;
};

std::vector<User> get_all_users();
std::optional<User> get_user_by_id(int id);
User create_user(std::string name, std::string email);
bool update_user(int id, std::string name, std::string email);
bool delete_user(int id);

#endif