import std.stdio;
import vibe.d;
import vibe.http.router;
import vibe.data.json;
import arsd.simplepg;
import std.conv;
import std.process;

struct User {
    int id;
    string name;
    string email;
}

Json toJSON(User u) {
    return Json([
        "id": u.id,
        "name": u.name,
        "email": u.email
    ]);
}

User fromJSON(Json j) {
    return User(
        j["id"].integer,
        j["name"].str,
        j["email"].str
    );
}

void main() {
    auto router = new URLRouter;
    auto settings = new HTTPServerSettings;
    settings.port = 8080;

    auto pg = new SimplePG(
        host: "host.docker.internal",
        port: 5432,
        database: "complang",
        user: "testuser",
        password: getenv("PGPASSWORD")
    );

    pg.connect();

    router.post("/users", (HTTPRequest req, HTTPResponse res) {
        req.bodyReader.readAll!(ubyte[])().then((ub) {
            auto body = cast(string)ub;
            Json jsonBody = parseJsonString(body);
            string name = jsonBody["name"].str;
            string email = jsonBody["email"].str;

            auto id = pg.insert("users", ["name", "email"], [name, email]);

            pg.query("SELECT id, name, email FROM users WHERE id = $1", [id], (PGResult r) {
                User user;
                r.fetchRow((int userId, string userName, string userEmail) {
                    user = User(userId, userName, userEmail);
                });
                res.writeJson(toJSON(user));
                res.status = HTTPStatus.created;
                res.finish();
            });
        }).wait();
    });

    router.get("/users", (HTTPRequest req, HTTPResponse res) {
        string query = "SELECT id, name, email FROM users";
        pg.query(query, [], (PGResult r) {
            Json[] usersJson;
            while (r.fetchRow((int id, string name, string email) {
                usersJson ~= toJSON(User(id, name, email));
            })) {}
            res.writeJson(usersJson);
            res.finish();
        });
    });

    router.get("/users/:id", (HTTPRequest req, HTTPResponse res, string id) {
        int userId = to!int(id);
        pg.query("SELECT id, name, email FROM users WHERE id = $1", [userId], (PGResult r) {
            if (r.rowCount == 0) {
                res.status = HTTPStatus.notFound;
                res.finish();
                return;
            }

            User user;
            r.fetchRow((int userId, string userName, string userEmail) {
                user = User(userId, userName, userEmail);
            });
            res.writeJson(toJSON(user));
            res.finish();
        });
    });

    router.put("/users/:id", (HTTPRequest req, HTTPResponse res, string id) {
        req.bodyReader.readAll!(ubyte[]).then((ub) {
            auto body = cast(string)ub;
            Json jsonBody = parseJsonString(body);
            string name = jsonBody["name"].str;
            string email = jsonBody["email"].str;
            int userId = to!int(id);

            int updated = pg.update("users", ["name", "email"], [name, email], "id = $1", [userId]);
            if (updated == 0) {
                res.status = HTTPStatus.notFound;
            } else {
                res.status = HTTPStatus.noContent;
            }
            res.finish();
        }).wait();
    });

    router.delete("/users/:id", (HTTPRequest req, HTTPResponse res, string id) {
        int userId = to!int(id);
        int deleted = pg.delete("users", "id = $1", [userId]);

        if (deleted == 0) {
            res.status = HTTPStatus.notFound;
        } else {
            res.status = HTTPStatus.noContent;
        }
        res.finish();
    });

    listenHTTP(settings, router);
    runApplication();
}