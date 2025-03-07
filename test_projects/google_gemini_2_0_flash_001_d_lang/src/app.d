
import vibe.d.core;
import vibe.d.web;
import vibe.d.web.rest;
import vibe.d.data.json;
import vibe.d.db.postgres;
import std.stdio;
import std.conv;
import std.process;

struct User {
    int id;
    string name;
    string email;
}

void main()
{
    auto settings = new HTTPServerSettings;
    settings.port = 8080;

    auto router = new URLRouter;

    router.post("/users", &createUser);
    router.get("/users", &listUsers);
    router.get("/users/:id", &getUser);
    router.put("/users/:id", &updateUser);
    router.delete("/users/:id", &deleteUser);

    listenHTTP(settings, router);
    runApplication();
}


string getConnectionString()
{
    string password = getenv("PGPASSWORD");
    if (password is null)
    {
        stderr.writeln("Error: PGPASSWORD environment variable not set.");
        assert(false); // crash if password is not present
    }
    return "host=host.docker.internal port=5432 dbname=complang user=testuser password=" ~ password;
}


@Route("/users")
void createUser(HTTPServerRequest req, HTTPServerResponse res)
{
    try {
        string body = req.readBody();
        auto userData = parseJson(body);
        string name = userData.name.str;
        string email = userData.email.str;

        auto connection = new PostgresConnection(getConnectionString());
        connection.open();

        auto statement = connection.prepare("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id");
        statement.execute(name, email);

        int userId;
        foreach (row; statement.fetchAllRows())
        {
            userId = row[0].get!long;
        }

        statement.close();
        connection.close();

        User newUser = User(userId, name, email);
        res.writeJson(newUser);
        res.statusCode = HTTPStatus.created;
    } catch (Exception e) {
        stderr.writeln("Error creating user: ", e.msg);
        res.statusCode = HTTPStatus.internalServerError;
        res.write("Internal Server Error");
    }
}


@Route("/users")
void listUsers(HTTPServerRequest req, HTTPServerResponse res)
{
    try {
        auto connection = new PostgresConnection(getConnectionString());
        connection.open();

        auto statement = connection.prepare("SELECT id, name, email FROM users");
        statement.execute();

        User[] users;
        foreach (row; statement.fetchAllRows())
        {
            User user = User(row[0].get!int, row[1].get!string, row[2].get!string);
            users ~= user;
        }

        statement.close();
        connection.close();

        res.writeJson(users);
        res.statusCode = HTTPStatus.ok;
    } catch (Exception e) {
        stderr.writeln("Error listing users: ", e.msg);
        res.statusCode = HTTPStatus.internalServerError;
        res.write("Internal Server Error");
    }
}


@Route("/users/:id")
void getUser(HTTPServerRequest req, HTTPServerResponse res, string id)
{
    try {
        int userId = to!int(id);

        auto connection = new PostgresConnection(getConnectionString());
        connection.open();

        auto statement = connection.prepare("SELECT id, name, email FROM users WHERE id = $1");
        statement.execute(userId);

        auto rows = statement.fetchAllRows();
        if (rows.length == 0)
        {
            res.statusCode = HTTPStatus.notFound;
            res.write("User not found");
            statement.close();
            connection.close();
            return;
        }

        User user = User(rows[0][0].get!int, rows[0][1].get!string, rows[0][2].get!string);

        statement.close();
        connection.close();

        res.writeJson(user);
        res.statusCode = HTTPStatus.ok;
    } catch (Exception e) {
        stderr.writeln("Error getting user: ", e.msg);
        res.statusCode = HTTPStatus.internalServerError;
        res.write("Internal Server Error");
    }
}


@Route("/users/:id")
void updateUser(HTTPServerRequest req, HTTPServerResponse res, string id)
{
    try {
        int userId = to!int(id);
        string body = req.readBody();
        auto userData = parseJson(body);
        string name = userData.name.str;
        string email = userData.email.str;

        auto connection = new PostgresConnection(getConnectionString());
        connection.open();

        auto statement = connection.prepare("UPDATE users SET name = $1, email = $2 WHERE id = $3");
        statement.execute(name, email, userId);

        if (statement.affectedRows == 0)
        {
            res.statusCode = HTTPStatus.notFound;
            res.write("User not found");
            statement.close();
            connection.close();
            return;
        }

        statement.close();
        connection.close();

        res.statusCode = HTTPStatus.noContent;
    } catch (Exception e) {
        stderr.writeln("Error updating user: ", e.msg);
        res.statusCode = HTTPStatus.internalServerError;
        res.write("Internal Server Error");
    }
}


@Route("/users/:id")
void deleteUser(HTTPServerRequest req, HTTPServerResponse res, string id)
{
    try {
        int userId = to!int(id);

        auto connection = new PostgresConnection(getConnectionString());
        connection.open();

        auto statement = connection.prepare("DELETE FROM users WHERE id = $1");
        statement.execute(userId);

        if (statement.affectedRows == 0)
        {
            res.statusCode = HTTPStatus.notFound;
            res.write("User not found");
            statement.close();
            connection.close();
            return;
        }

        statement.close();
        connection.close();

        res.statusCode = HTTPStatus.noContent;
    } catch (Exception e) {
        stderr.writeln("Error deleting user: ", e.msg);
        res.statusCode = HTTPStatus.internalServerError;
        res.write("Internal Server Error");
    }
}
