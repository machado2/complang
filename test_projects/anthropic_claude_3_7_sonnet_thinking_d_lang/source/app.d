
import vibe.vibe;
import vibe.db.postgresql;
import std.process : environment;
import std.conv : to;
import std.stdio;

void main()
{
    auto settings = new HTTPServerSettings;
    settings.port = 8080;
    settings.bindAddresses = ["0.0.0.0"];

    // Get PostgreSQL password from environment variable
    string pgPassword = environment.get("PGPASSWORD", "");
    if (pgPassword == "") {
        logWarn("PGPASSWORD environment variable not set");
    }

    // Connect to PostgreSQL
    auto pgSettings = new PostgresClientSettings;
    pgSettings.host = "host.docker.internal";
    pgSettings.port = 5432;
    pgSettings.database = "complang";
    pgSettings.username = "testuser";
    pgSettings.password = pgPassword;
    
    auto postgres = new PostgresClient(pgSettings);
    scope(exit) postgres.disconnect();
    
    try {
        postgres.connect();
        logInfo("Connected to PostgreSQL database");
    } catch (Exception e) {
        logError("Failed to connect to PostgreSQL database: %s", e.msg);
        return;
    }
    
    auto router = new URLRouter;

    // POST /users - Create a user
    router.post("/users", (HTTPServerRequest req, HTTPServerResponse res) {
        auto json = req.json;
        
        if ("name" !in json || "email" !in json) {
            res.statusCode = HTTPStatus.badRequest;
            res.writeJsonBody(["error": "Name and email are required"]);
            return;
        }

        string name = json["name"].get!string;
        string email = json["email"].get!string;

        try {
            auto command = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email";
            auto result = postgres.queryRow(command, name, email);
            
            int id = result["id"].as!int;
            
            res.statusCode = HTTPStatus.created;
            res.writeJsonBody([
                "id": id,
                "name": name,
                "email": email
            ]);
        } catch (Exception e) {
            res.statusCode = HTTPStatus.internalServerError;
            res.writeJsonBody(["error": "Failed to create user: " ~ e.msg]);
        }
    });

    // GET /users - List all users
    router.get("/users", (HTTPServerRequest req, HTTPServerResponse res) {
        try {
            auto command = "SELECT id, name, email FROM users ORDER BY id";
            auto result = postgres.query(command);
            
            Json[] users;
            foreach (row; result) {
                users ~= Json([
                    "id": Json(row["id"].as!int),
                    "name": Json(row["name"].as!string),
                    "email": Json(row["email"].as!string)
                ]);
            }
            
            res.writeJsonBody(users);
        } catch (Exception e) {
            res.statusCode = HTTPStatus.internalServerError;
            res.writeJsonBody(["error": "Failed to retrieve users: " ~ e.msg]);
        }
    });

    // GET /users/{id} - Get a single user
    router.get("/users/:id", (HTTPServerRequest req, HTTPServerResponse res) {
        string idParam = req.params["id"];
        int id;
        
        try {
            id = to!int(idParam);
        } catch (Exception e) {
            res.statusCode = HTTPStatus.badRequest;
            res.writeJsonBody(["error": "Invalid user ID"]);
            return;
        }

        try {
            auto command = "SELECT id, name, email FROM users WHERE id = $1";
            auto result = postgres.query(command, id);
            
            if (result.empty) {
                res.statusCode = HTTPStatus.notFound;
                res.writeJsonBody(["error": "User not found"]);
                return;
            }
            
            auto row = result.front;
            res.writeJsonBody([
                "id": row["id"].as!int,
                "name": row["name"].as!string,
                "email": row["email"].as!string
            ]);
        } catch (Exception e) {
            res.statusCode = HTTPStatus.internalServerError;
            res.writeJsonBody(["error": "Failed to retrieve user: " ~ e.msg]);
        }
    });

    // PUT /users/{id} - Update a user
    router.put("/users/:id", (HTTPServerRequest req, HTTPServerResponse res) {
        string idParam = req.params["id"];
        int id;
        
        try {
            id = to!int(idParam);
        } catch (Exception e) {
            res.statusCode = HTTPStatus.badRequest;
            res.writeJsonBody(["error": "Invalid user ID"]);
            return;
        }

        auto json = req.json;
        
        if ("name" !in json || "email" !in json) {
            res.statusCode = HTTPStatus.badRequest;
            res.writeJsonBody(["error": "Name and email are required"]);
            return;
        }

        string name = json["name"].get!string;
        string email = json["email"].get!string;

        try {
            auto command = "UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id";
            auto result = postgres.query(command, name, email, id);
            
            if (result.empty) {
                res.statusCode = HTTPStatus.notFound;
                res.writeJsonBody(["error": "User not found"]);
                return;
            }
            
            res.statusCode = HTTPStatus.noContent;
        } catch (Exception e) {
            res.statusCode = HTTPStatus.internalServerError;
            res.writeJsonBody(["error": "Failed to update user: " ~ e.msg]);
        }
    });

    // DELETE /users/{id} - Delete a user
    router.delete_("/users/:id", (HTTPServerRequest req, HTTPServerResponse res) {
        string idParam = req.params["id"];
        int id;
        
        try {
            id = to!int(idParam);
        } catch (Exception e) {
            res.statusCode = HTTPStatus.badRequest;
            res.writeJsonBody(["error": "Invalid user ID"]);
            return;
        }

        try {
            auto command = "DELETE FROM users WHERE id = $1 RETURNING id";
            auto result = postgres.query(command, id);
            
            if (result.empty) {
                res.statusCode = HTTPStatus.notFound;
                res.writeJsonBody(["error": "User not found"]);
                return;
            }
            
            res.statusCode = HTTPStatus.noContent;
        } catch (Exception e) {
            res.statusCode = HTTPStatus.internalServerError;
            res.writeJsonBody(["error": "Failed to delete user: " ~ e.msg]);
        }
    });

    listenHTTP(settings, router);
    runApplication();
}
