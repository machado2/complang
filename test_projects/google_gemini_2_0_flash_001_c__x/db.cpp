#include "db.h"
#include <cstdlib>

pqxx::connection get_db_connection() {
    const char* password = std::getenv("PGPASSWORD");
    if (password == nullptr) {
        std::cerr << "PGPASSWORD environment variable not set!" << std::endl;
        throw std::runtime_error("PGPASSWORD environment variable not set!");
    }

    std::string connection_string = "host=host.docker.internal port=5432 dbname=complang user=testuser password=" + std::string(password);
    return pqxx::connection(connection_string);
}