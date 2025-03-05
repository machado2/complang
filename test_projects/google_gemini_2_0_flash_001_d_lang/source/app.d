import vibe.d.web.server;
import vibe.d.web.middleware;
import vibe.d.web.middleware.cors;
import vibe.d.core.log;
import vibe.d.data.json;
import std.conv;
import std.string;
import std.stdio;
import std.algorithm;
import postgresql;

void main()
{
	logInfo("Starting server...");

	auto settings = new HTTPServerSettings;
	settings.port = 8080;

	settings.middleware = new CORSMiddleware();

	auto router = new URLRouter();

	router.get("/users", &getUsers);
	router.post("/users", &createUser);
	router.get("/users/:id", &getUser);
	router.put("/users/:id", &updateUser);
	router.delete("/users/:id", &deleteUser);

	auto db = connectPG();

	onInterface(scopeGuard {
        db.close();
    });

	listenHTTP(settings, router);
	vibe.d.core.run();
}

PGConnection connectPG()
{
    string connectionString = "host=host.docker.internal port=5432 dbname=test_google_gemini_2_0_flash_001_d_lang user=postgres password=" ~ getenv("PGPASSWORD");
    PGConnection db;
    try {
        db = new PGConnection(connectionString);
        db.execute("CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, name TEXT, email TEXT)");
    } catch (PGException e) {
        logError("Error connecting to Postgres: %s", e.msg);
        throw e;
    }
    return db;
}

void getUsers(HTTPServerRequest req, HTTPServerResponse res)
{
    auto db = connectPG();
    scope(exit) db.close();
    auto result = db.query("SELECT id, name, email FROM users");
    auto users = result.map!(row => Json(["id": row[0].to!int, "name": row[1].to!string, "email": row[2].to!string])).array;
    res.writeJson(users);
}

void createUser(HTTPServerRequest req, HTTPServerResponse res)
{
    Json data = req.readJson();
    string name = data.get!string("name");
    string email = data.get!string("email");

    auto db = connectPG();
    scope(exit) db.close();
    auto result = db.queryParams("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email", name, email);
    auto user = Json(["id": result[0][0].to!int, "name": result[0][1].to!string, "email": result[0][2].to!string]);
    res.writeJson(user);
    res.status = HTTPStatus.created;
}

void getUser(HTTPServerRequest req, HTTPServerResponse res, string id)
{
    int userId = to!int(id);
    auto db = connectPG();
    scope(exit) db.close();
    auto result = db.queryParams("SELECT id, name, email FROM users WHERE id = $1", userId);
    if (result.empty)
    {
        res.status = HTTPStatus.notFound;
        return;
    }
    auto user = Json(["id": result[0][0].to!int, "name": result[0][1].to!string, "email": result[0][2].to!string]);
    res.writeJson(user);
}

void updateUser(HTTPServerRequest req, HTTPServerResponse res, string id)
{
    int userId = to!int(id);
    Json data = req.readJson();
    string name = data.get!string("name");
    string email = data.get!string("email");

    auto db = connectPG();
    scope(exit) db.close();
    auto result = db.queryParams("UPDATE users SET name = $1, email = $2 WHERE id = $3", name, email, userId);
    if (result.rowsAffected == 0)
    {
        res.status = HTTPStatus.notFound;
        return;
    }
    res.status = HTTPStatus.noContent;
}

void deleteUser(HTTPServerRequest req, HTTPServerResponse res, string id)
{
    int userId = to!int(id);
    auto db = connectPG();
    scope(exit) db.close();
    auto result = db.queryParams("DELETE FROM users WHERE id = $1", userId);
    if (result.rowsAffected == 0)
    {
        res.status = HTTPStatus.notFound;
        return;
    }
    res.status = HTTPStatus.noContent;
}
