
import vibe.d.core;
import vibe.d.web;
import vibe.d.web.rest;
import vibe.d.web.json;
import vibe.data.json;
import vibe.db.database;
import vibe.db.query;
import vibe.db.postgres;
import std.conv;
import std.string;
import std.stdio;
import std.algorithm;

struct User {
    int id;
    string name;
    string email;
}

void main() {
    auto settings = new HTTPServerSettings();
    settings.port = 8080;

    auto router = new URLRouter;

    // Connect to PostgreSQL database
    auto db = new Postgres("host=host.docker.internal port=5432 dbname=complang user=testuser password=" ~ getenv("PGPASSWORD"));
    db.connect();


    router.post!("/users", (HTTPRequest req, HTTPResponse res) {
        req.readJsonBody!(JSONValue)().then((json) {
            string name = json["name"].str;
            string email = json["email"].str;

            string insertQuery = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email;";
            auto result = db.query(insertQuery, name, email);

            result.then!((rows) {
                if (rows.length > 0) {
                    auto row = rows[0];
                    User user = User(row["id"].get!int(), row["name"].get!string(), row["email"].get!string());

                    res.writeJson(user);
                    res.statusCode = HTTPStatus.created;
                    res.finish();
                } else {
                    res.statusCode = HTTPStatus.internalServerError;
                    res.write("Insert failed");
                    res.finish();
                }
            }).except!((Exception e) {
               stderr.writeln("Exception during insert: ", e.msg);
               res.statusCode = HTTPStatus.internalServerError;
               res.write("Internal server error during insert");
               res.finish();
            });


        }).except!((Exception e) {
            stderr.writeln("Exception reading JSON: ", e.msg);
            res.statusCode = HTTPStatus.badRequest;
            res.write("Bad request: invalid JSON");
            res.finish();
        });
    });

    router.get!("/users", (HTTPRequest req, HTTPResponse res) {
       string selectQuery = "SELECT id, name, email FROM users;";
       auto result = db.query(selectQuery);

       result.then!((rows) {
           JSONValue[] users;
           foreach (row; rows) {
               User user = User(row["id"].get!int(), row["name"].get!string(), row["email"].get!string());
               users ~= toJson(user);
           }
           res.writeJson(users);
           res.finish();
       }).except!((Exception e) {
           stderr.writeln("Exception during select all: ", e.msg);
           res.statusCode = HTTPStatus.internalServerError;
           res.write("Internal server error during select all");
           res.finish();
       });
    });

    router.get!("/users/:id", (HTTPRequest req, HTTPResponse res, string id) {
       int userId = to!int(id);
       string selectQuery = "SELECT id, name, email FROM users WHERE id = $1;";
       auto result = db.query(selectQuery, userId);

       result.then!((rows) {
           if (rows.length > 0) {
               auto row = rows[0];
               User user = User(row["id"].get!int(), row["name"].get!string(), row["email"].get!string());
               res.writeJson(user);
               res.finish();
           } else {
               res.statusCode = HTTPStatus.notFound;
               res.write("User not found");
               res.finish();
           }
       }).except!((Exception e) {
           stderr.writeln("Exception during select one: ", e.msg);
           res.statusCode = HTTPStatus.internalServerError;
           res.write("Internal server error during select one");
           res.finish();
       });
    });


   router.put!("/users/:id", (HTTPRequest req, HTTPResponse res, string id) {
        req.readJsonBody!(JSONValue)().then((json) {
            int userId = to!int(id);
            string name = json["name"].str;
            string email = json["email"].str;

            string updateQuery = "UPDATE users SET name = $1, email = $2 WHERE id = $3;";
            auto result = db.execute(updateQuery, name, email, userId);

            result.then!((affectedRows) {
                if (affectedRows > 0) {
                    res.statusCode = HTTPStatus.noContent; // or HTTPStatus.ok
                    res.finish();
                } else {
                    res.statusCode = HTTPStatus.notFound;
                    res.write("User not found");
                    res.finish();
                }
            }).except!((Exception e) {
                stderr.writeln("Exception during update: ", e.msg);
                res.statusCode = HTTPStatus.internalServerError;
                res.write("Internal server error during update");
                res.finish();
            });
        }).except!((Exception e) {
            stderr.writeln("Exception during JSON read on update: ", e.msg);
            res.statusCode = HTTPStatus.badRequest;
            res.write("Bad request: invalid JSON");
            res.finish();
        });
    });

    router.delete!("/users/:id", (HTTPRequest req, HTTPResponse res, string id) {
        int userId = to!int(id);
        string deleteQuery = "DELETE FROM users WHERE id = $1;";
        auto result = db.execute(deleteQuery, userId);

        result.then!((affectedRows) {
            if (affectedRows > 0) {
                res.statusCode = HTTPStatus.noContent; // or HTTPStatus.ok
                res.finish();
            } else {
                res.statusCode = HTTPStatus.notFound;
                res.write("User not found");
                res.finish();
            }
        }).except!((Exception e) {
            stderr.writeln("Exception during delete: ", e.msg);
            res.statusCode = HTTPStatus.internalServerError;
            res.write("Internal server error during delete");
            res.finish();
        });
    });



    listenHTTP(settings, router);

    // Keep the application running
    scope guard {
        db.close();
    }

    runApplication();
}
