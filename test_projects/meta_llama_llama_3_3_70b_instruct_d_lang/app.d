
import std.array;
import std.conv;
import std.datetime;
import std.exception;
import std.format;
import std.json;
import std.net.curl;
import std.path;
import std.range;
import std.regex;
import std.stdio;
import std.string;
import std.uri;
import std.uuid;

import vibe.d;
import vibe.db.postgres;

struct User
{
    int id;
    string name;
    string email;
}

void main()
{
    auto settings = new PostgresSettings("host.docker.internal", 5432, "complang", "testuser", getenv("PGPASSWORD"));
    auto db = connectPostgres(settings);

    auto router = new URLRouter;
    router.any("/users", &handleUsers);
    router.any("/users/:id", &handleUser);

    auto listener = listenHTTP(settings, "0.0.0.0", 8080, router);

    runApplication();
}

void handleUsers(HTTPServerRequest req, HTTPServerResponse res)
{
    switch(req.method)
    {
        case HTTPMethod.POST:
            handleCreateUser(req, res);
            break;
        case HTTPMethod.GET:
            handleGetUsers(req, res);
            break;
        default:
            res.statusCode = HTTPStatus.MethodNotAllowed;
            res.writeBody("Method not allowed");
    }
}

void handleUser(HTTPServerRequest req, HTTPServerResponse res)
{
    auto userId = req.params["id"].to!int;

    switch(req.method)
    {
        case HTTPMethod.GET:
            handleGetUser(req, res, userId);
            break;
        case HTTPMethod.PUT:
            handleUpdateUser(req, res, userId);
            break;
        case HTTPMethod.DELETE:
            handleDeleteUser(req, res, userId);
            break;
        default:
            res.statusCode = HTTPStatus.MethodNotAllowed;
            res.writeBody("Method not allowed");
    }
}

void handleCreateUser(HTTPServerRequest req, HTTPServerResponse res)
{
    auto json = parseJson(req.bodyReader.readAll());
    auto name = json["name"].str();
    auto email = json["email"].str();

    auto dbConnection = connectPostgres(new PostgresSettings("host.docker.internal", 5432, "complang", "testuser", getenv("PGPASSWORD")));
    auto command = dbConnection.cmd("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *");
    command.execute(name, email);

    auto result = command.getResult();
    auto userId = result[0][0].to!int;
    auto createdUser = User(userId, name, email);

    res.statusCode = HTTPStatus.Created;
    res.writeBody(toJson(createdUser));
}

void handleGetUsers(HTTPServerRequest req, HTTPServerResponse res)
{
    auto dbConnection = connectPostgres(new PostgresSettings("host.docker.internal", 5432, "complang", "testuser", getenv("PGPASSWORD")));
    auto command = dbConnection.cmd("SELECT * FROM users");
    auto result = command.execute().result;

    auto users = result.map!(row => User(row[0].to!int, row[1].to!string, row[2].to!string)).array();

    res.writeBody(toJson(users));
}

void handleGetUser(HTTPServerRequest req, HTTPServerResponse res, int userId)
{
    auto dbConnection = connectPostgres(new PostgresSettings("host.docker.internal", 5432, "complang", "testuser", getenv("PGPASSWORD")));
    auto command = dbConnection.cmd("SELECT * FROM users WHERE id = $1");
    command.execute(userId);

    auto result = command.getResult();

    if(!result.empty)
    {
        auto user = User(result[0][0].to!int, result[0][1].to!string, result[0][2].to!string);
        res.writeBody(toJson(user));
    }
    else
    {
        res.statusCode = HTTPStatus.NotFound;
        res.writeBody("User not found");
    }
}

void handleUpdateUser(HTTPServerRequest req, HTTPServerResponse res, int userId)
{
    auto json = parseJson(req.bodyReader.readAll());
    auto name = json["name"].str();
    auto email = json["email"].str();

    auto dbConnection = connectPostgres(new PostgresSettings("host.docker.internal", 5432, "complang", "testuser", getenv("PGPASSWORD")));
    auto command = dbConnection.cmd("UPDATE users SET name = $1, email = $2 WHERE id = $3");
    command.execute(name, email, userId);

    if(command.affectedRows() > 0)
    {
        res.writeBody("User updated");
    }
    else
    {
        res.statusCode = HTTPStatus.NotFound;
        res.writeBody("User not found");
    }
}

void handleDeleteUser(HTTPServerRequest req, HTTPServerResponse res, int userId)
{
    auto dbConnection = connectPostgres(new PostgresSettings("host.docker.internal", 5432, "complang", "testuser", getenv("PGPASSWORD")));
    auto command = dbConnection.cmd("DELETE FROM users WHERE id = $1");
    command.execute(userId);

    if(command.affectedRows() > 0)
    {
        res.writeBody("User deleted");
    }
    else
    {
        res.statusCode = HTTPStatus.NotFound;
        res.writeBody("User not found");
    }
}
