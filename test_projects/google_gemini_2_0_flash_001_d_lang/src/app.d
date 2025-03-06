module app;

import vibe.d.core;
import vibe.d.web;
import vibe.d.web.rest;
import vibe.d.web.middleware;
import vibe.d.web.middleware.cors;
import vibe.d.serialization;
import vibe.d.json;

import std.stdio;
import std.conv;
import std.string;
import std.format;
import std.algorithm;
import std.range;

import dietopia.postgres;

struct User
{
    int id;
    string name;
    string email;
}

auto pg = new Postgres("host=host.docker.internal port=5432 dbname=complang user=testuser password=$PGPASSWORD");

void main()
{
    auto settings = new HTTPServerSettings;
    settings.port = 8080;

    auto router = new URLRouter;

    // CORS middleware (for development)
    auto cors = new CORSMiddleware;
    cors.allowOrigin = CORSMiddleware.AllowOrigin(
        origins: [ "*" ]
    );

    auto app = new AutoRouter;
    app.use(cors);
    app.get("/users", &getUsers);
    app.post("/users", &createUser);
    app.get("/users/:id", &getUser);
    app.put("/users/:id", &updateUser);
    app.delete("/users/:id", &deleteUser);


    router.registerRestInterface(app);
    
    listenHTTP(settings, router.middleware);
    
    writeln("Server listening on port 8080");
    runApplication();
}


string escape(string str)
{
    string result;
    foreach (dchar c in str)
    {
        switch (c)
        {
            case '\\': result ~= '\\'; break;
            case '\'': result ~= '\''; break;
            case '"': result ~= '\''; break;
            case '\n': result ~= '\n'; break;
            case '\r': result ~= '\r'; break;
            case '\t': result ~= '\t'; break;
            default: result ~= c;
        }
    }
    return result;
}

// GET /users
@safe auto getUsers(HTTPServerRequest req, HTTPServerResponse res) {
    pg.query('SELECT id, name, email FROM users');
    User[] users;
    foreach (row; pg.rows) {
        users ~= User(row[0].to!int, row[1].to!string, row[2].to!string);
    }
    return res.json(users);
}

// POST /users
@safe auto createUser(HTTPServerRequest req, HTTPServerResponse res) {
    
    req.jsonBody!(User).then!((User user) {
        string name = escape(user.name);
        string email = escape(user.email);
        auto query = format('INSERT INTO users (name, email) VALUES (\"%s\", \"%s\") RETURNING id, name, email', name, email);
        pg.query(query);
        
        if (pg.rows.length > 0) {
            auto row = pg.rows[0];
            User createdUser = User(row[0].to!int, row[1].to!string, row[2].to!string);
            res.statusCode(201);
            res.json(createdUser);
        } else {
            res.statusCode(500);
            res.writeBody("Failed to create user");
        }
    }).then!((_){
         pg.finish();
    }).catch!((Exception e){
        stderr.writeln(e.msg);
        res.statusCode(500);
        res.writeBody("Failed to parse JSON: " ~ e.msg);
    });
}

// GET /users/:id
@safe auto getUser(HTTPServerRequest req, HTTPServerResponse res, string id) {
    int userId = to!int(id);
    pg.query(format('SELECT id, name, email FROM users WHERE id = %s', userId));
    if (pg.rows.length > 0) {
        auto row = pg.rows[0];
        User user = User(row[0].to!int, row[1].to!string, row[2].to!string);
        res.json(user);
    } else {
        res.statusCode(404);
    }
     pg.finish();
}

// PUT /users/:id
@safe auto updateUser(HTTPServerRequest req, HTTPServerResponse res, string id) {
    int userId = to!int(id);
    req.jsonBody!(User).then!((User user) {
        pg.query(format('UPDATE users SET name = \"%s\", email = \"%s\" WHERE id = %s', escape(user.name), escape(user.email), userId));
        if (pg.affectedRows > 0) {
            res.statusCode(204);
        } else {
            res.statusCode(404);
        }
        pg.finish();
    }).catch!((Exception e){
        stderr.writeln(e.msg);
        res.statusCode(500);
        res.writeBody("Failed to parse JSON: " ~ e.msg);
    });
}

// DELETE /users/:id
@safe auto deleteUser(HTTPServerRequest req, HTTPServerResponse res, string id) {
    int userId = to!int(id);
    pg.query(format('DELETE FROM users WHERE id = %s', userId));
    if (pg.affectedRows > 0) {
        res.statusCode(204);
    } else {
        res.statusCode(404);
    }
     pg.finish();
}
