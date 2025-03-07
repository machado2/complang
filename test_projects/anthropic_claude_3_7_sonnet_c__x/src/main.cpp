#include <iostream>
#include <cstdlib>
#include <cstring>
#include <crow.h>
#include "db_handler.h"
#include "user_routes.h"

int main() {
    // Create database handler
    DBHandler db;
    
    // Get database password from environment
    const char* password = std::getenv("PGPASSWORD");
    if (!password) {
        std::cerr << "Error: PGPASSWORD environment variable not set" << std::endl;
        return 1;
    }
    
    // Connect to the database
    bool connected = db.connect("host.docker.internal", "5432", "complang", "testuser", password);
    if (!connected) {
        std::cerr << "Failed to connect to the database" << std::endl;
        return 1;
    }
    
    std::cout << "Connected to PostgreSQL database successfully!" << std::endl;
    
    // Create Crow application
    crow::App<> app;
    
    // Register user routes
    UserRoutes::registerRoutes(app, db);
    
    // Set up server
    app.port(8080)
        .multithreaded()
        .run();
    
    return 0;
}
