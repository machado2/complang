#pragma once

#include <crow.h>
#include "db_handler.h"

namespace UserRoutes {
    void registerRoutes(crow::App<>& app, DBHandler& db);
}
