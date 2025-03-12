import vibe.vibe;
import vibe.db.pg;
import std.json;
import std.conv;
import std.exception;
import std.process;
import std.stdio;

// Global connection pool variable
PgConnectionPool pool;

// Setup the PostgreSQL connection pool using the PGPASSWORD env var
PgConnectionPool setupDb() {
    string pgPass = getenv("PGPASSWORD");
    enforce(pgPass.length > 0, "Environment variable PGPASSWORD must be set");
    // Connect using the host.docker.internal hostname for Docker, database 'complang', user 'testuser'
    string connStr = "postgresql://testuser:" ~ pgPass ~ "@host.docker.internal:5432/complang";
    auto pool = new PgConnectionPool(connStr);
    return pool;
}

void main() {
    pool = setupDb();
    auto router = new URLRouter;

    // POST /users : Create a new user
    router.post("/users", (HTTPServerRequest req, HTTPServerResponse res) {
        string body = req.readEntireBody();
        JSONValue data = parseJSON(body);
        string name = data["name"].str;
        string email = data["email"].str;
        // Insert into the database and return the newly generated id.
        auto result = pool.execute("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id", [name, email]);
        if (result.length == 0) {
            res.status = HTTPStatus.internalServerError;
            res.writeBody("Failed to create user");
            return;
        }
        int newId = result[0][0].to!int;
        res.status = HTTPStatus.created;
        res.writeBody(`{"id":` ~ to!string(newId) ~ `,"name":"` ~ name ~ `","email":"` ~ email ~ `"}`);
    });

    // GET /users : Retrieve all users
    router.get("/users", (HTTPServerRequest req, HTTPServerResponse res) {
        auto result = pool.execute("SELECT id, name, email FROM users");
        string json = "[";
        bool first = true;
        foreach (row; result) {
            if (!first) { json ~= ","; } else { first = false; }
            int id = row[0].to!int;
            string name = row[1].to!string;
            string email = row[2].to!string;
            json ~= `{"id":` ~ to!string(id) ~ `,"name":"` ~ name ~ `","email":"` ~ email ~ `"}`;
        }
        json ~= "]";
        res.writeBody(json);
    });

    // GET /users/{id} : Retrieve a single user
    router.get("^/users/([0-9]+)$", (HTTPServerRequest req, HTTPServerResponse res, string idStr) {
        int id = to!int(idStr);
        auto result = pool.execute("SELECT id, name, email FROM users WHERE id = $1", [id]);
        if (result.length == 0) {
            res.status = HTTPStatus.notFound;
            res.writeBody("User not found");
            return;
        }
        auto row = result[0];
        int userId = row[0].to!int;
        string name = row[1].to!string;
        string email = row[2].to!string;
        res.writeBody(`{"id":` ~ to!string(userId) ~ `,"name":"` ~ name ~ `","email":"` ~ email ~ `"}`);
    });

    // PUT /users/{id} : Update a user
    router.put("^/users/([0-9]+)$", (HTTPServerRequest req, HTTPServerResponse res, string idStr) {
        int id = to!int(idStr);
        string body = req.readEntireBody();
        JSONValue data = parseJSON(body);
        string name = data["name"].str;
        string email = data["email"].str;
        auto result = pool.execute("UPDATE users SET name=$1, email=$2 WHERE id=$3", [name, email, id]);
        // Check if any row was updated (assumes result provides 'affectedRows')
        if (result.affectedRows == 0) {
            res.status = HTTPStatus.notFound;
            res.writeBody("User not found");
            return;
        }
        res.status = HTTPStatus.noContent;
    });

    // DELETE /users/{id} : Delete a user
    router.delete("^/users/([0-9]+)$", (HTTPServerRequest req, HTTPServerResponse res, string idStr) {
        int id = to!int(idStr);
        auto result = pool.execute("DELETE FROM users WHERE id=$1", [id]);
        if (result.affectedRows == 0) {
            res.status = HTTPStatus.notFound;
            res.writeBody("User not found");
            return;
        }
        res.status = HTTPStatus.noContent;
    });

    // Start the HTTP server on port 8080.
    listenHTTP(new HTTPServerSettings(port:8080), router);
    runApplication();
}
