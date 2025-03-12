
#ifndef DB_H
#define DB_H

#include <string>
#include <vector>
#include <memory>

#include "user.h"

namespace pqxx {
    class connection;
}

class DB {
public:
    DB(const std::string& host, int port, const std::string& dbname, const std::string& user, const std::string& password);
    ~DB();

    bool is_open() const;

    User create_user(const std::string& name, const std::string& email);
    User get_user(int id);
    std::vector<User> get_all_users();
    bool update_user(int id, const std::string& name, const std::string& email);
    bool delete_user(int id);

private:
    std::string host;
    int port;
    std::string dbname;
    std::string user;
    std::string password;
    std::unique_ptr<pqxx::connection> conn;
};

#endif
