#ifndef DB_H
#define DB_H

#include <pqxx/pqxx>
#include <iostream>
#include <string>
#include <optional>
#include <vector>

class User;

pqxx::connection get_db_connection();

#endif