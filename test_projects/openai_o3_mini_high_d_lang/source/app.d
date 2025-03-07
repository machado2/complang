module app;

import vibe.vibe;
import vibe.db.postgresql;
import std.env;
import std.conv;
import std.stdio;
import std.json; // For JSON handling

// Global PostgreSQL connection pool.
PostgreSQLConnectionPool dbPool;

void main()
{
    // Get the PostgreSQL password from the environment.
    auto pgPassword = environment.get("PGPASSWORD");
    if (!pgPassword.isSome)
    {
        writeln("PGPASSWORD environment variable not set");
        return;
    }
    string password = pgPassword.get;

    // Initialize the PostgreSQL connection pool.
    // Connection string format: "dbname=complang user=testuser password=... host=host.docker.internal port=5432"
    dbPool = new PostgreSQLConnectionPool("dbname=complang user=testuser password=" ~ password ~ " host=host.docker.internal port=5432");

    // Set up URL router with endpoints.
    auto router = new URLRouter();
    router.handlePost("/users", &createUser);
    router.handleGet("/users", &getUsers);
    router.handleGet("/users/{id}", &getUser);
    router.handlePut("/users/{id}", &updateUser);
    router.handleDelete("/users/{id}", &deleteUser);

    // Listen on port 8080.
    listenHTTP(new HTTPServerSettings(8080), router);
    runApplication();
}

void createUser(HTTPServerRequest req, HTTPServerResponse res)
{
    // Parse the JSON payload.
    auto payload = req.readJson();
    if (payload.type != JSONType.JSON_OBJECT)
    {
        res.statusCode = 400;
        res.writeBody("Invalid JSON payload");
        return;
    }
    string name, email;
    try {
        name = payload["name"].str;
        email = payload["email"].str;
    } catch(Exception e) {
        res.statusCode = 400;
        res.writeBody("Missing 'name' or 'email' fields");
        return;
    }
    
    auto conn = dbPool.getConnection();
    // Insert the new user and get the generated id.
    auto result = conn.query("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id", name, email);
    if (result.length == 0)
    {
        res.statusCode = 500;
        res.writeBody("Failed to create user");
        return;
    }
    int id = cast(int) result[0][0];
    res.statusCode = 201;
    res.writeJson(["id": id, "name": name, "email": email]);
}

void getUsers(HTTPServerRequest req, HTTPServerResponse res)
{
    auto conn = dbPool.getConnection();
    auto result = conn.query("SELECT id, name, email FROM users");
    auto usersList = [];
    foreach(row; result) {
        usersList ~= ["id": cast(int) row[0], "name": row[1].to!string, "email": row[2].to!string];
    }
    res.writeJson(usersList);
}

void getUser(HTTPServerRequest req, HTTPServerResponse res)
{
    string idStr = req.params["id"];
    int id;
    try {
        id = to!int(idStr);
    } catch(Exception e) {
        res.statusCode = 400;
        res.writeBody("Invalid user id");
        return;
    }
    auto conn = dbPool.getConnection();
    auto result = conn.query("SELECT id, name, email FROM users WHERE id=$1", id);
    if (result.length == 0)
    {
        res.statusCode = 404;
        res.writeBody("User not found");
        return;
    }
    auto row = result[0];
    res.writeJson(["id": cast(int) row[0], "name": row[1].to!string, "email": row[2].to!string]);
}

void updateUser(HTTPServerRequest req, HTTPServerResponse res)
{
    string idStr = req.params["id"];
    int id;
    try {
        id = to!int(idStr);
    } catch(Exception e) {
        res.statusCode = 400;
        res.writeBody("Invalid user id");
        return;
    }
    auto payload = req.readJson();
    if (payload.type != JSONType.JSON_OBJECT)
    {
        res.statusCode = 400;
        res.writeBody("Invalid JSON payload");
        return;
    }
    string name, email;
    try {
        name = payload["name"].str;
        email = payload["email"].str;
    } catch(Exception e) {
        res.statusCode = 400;
        res.writeBody("Missing 'name' or 'email' fields");
        return;
    }
    auto conn = dbPool.getConnection();
    int affected = conn.execute("UPDATE users SET name=$1, email=$2 WHERE id=$3", name, email, id);
    if (affected == 0)
    {
        res.statusCode = 404;
        res.writeBody("User not found");
    }
    else {
        res.statusCode = 200;
    }
}

void deleteUser(HTTPServerRequest req, HTTPServerResponse res)
{
    string idStr = req.params["id"];
    int id;
    try {
        id = to!int(idStr);
    } catch(Exception e) {
        res.statusCode = 400;
        res.writeBody("Invalid user id");
        return;
    }
    auto conn = dbPool.getConnection();
    int affected = conn.execute("DELETE FROM users WHERE id=$1", id);
    if (affected == 0)
    {
        res.statusCode = 404;
        res.writeBody("User not found");
    }
    else {
        res.statusCode = 200;
    }
}
