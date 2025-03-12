
import vibe.d;
import std.stdio;
import std.string;
import std.conv;
import std.json;
import std.process;
import std.format;

import database.postgresql;

struct User {
    int id;
    string name;
    string email;
}

void main() {
    auto settings = new PgSettings;
    settings.host = "host.docker.internal";
    settings.port = 5432;
    settings.user = "testuser";
    settings.password = getenv("PGPASSWORD"); // Use the environment variable
    settings.database = "complang";

    auto pool = new PgConnectionPool(settings);

    auto router = new URLRouter;

    router.post("/users", async (scope) {
        scope.requireJson();
        auto json = scope.json;

        string name = json["name"].str;
        string email = json["email"].str;

        try {
            auto conn = await pool.get();
            scope.onClose(() => pool.release(conn));

            string insertQuery = format("INSERT INTO users (name, email) VALUES ('%s', '%s') RETURNING id", name, email);
            auto result = await conn.query(insertQuery);

            int id = result[0]["id"].get!int;

            auto user = User(id, name, email);
            scope.response.writeJson(user);
            scope.response.status = HTTPStatus.created;

        } catch (Exception e) {
            scope.response.status = HTTPStatus.internalServerError;
            scope.response.write("Error creating user: " ~ e.msg);
        }
    });

    router.get("/users", async (scope) {
        try {
            auto conn = await pool.get();
            scope.onClose(() => pool.release(conn));

            string query = "SELECT id, name, email FROM users";
            auto result = await conn.query(query);

            Json[] users;
            foreach (row; result) {
                users ~= Json([
                    "id": row["id"].get!int,
                    "name": row["name"].get!string,
                    "email": row["email"].get!string
                ]);
            }

            scope.response.writeJson(users);
        } catch (Exception e) {
            scope.response.status = HTTPStatus.internalServerError;
            scope.response.write("Error getting users: " ~ e.msg);
        }
    });

    router.get("/users/:id", async (scope, string id) {
        try {
            auto conn = await pool.get();
            scope.onClose(() => pool.release(conn));

            string query = format("SELECT id, name, email FROM users WHERE id = %s", id);
            auto result = await conn.query(query);

            if (result.length == 0) {
                scope.response.status = HTTPStatus.notFound;
                scope.response.write("User not found");
            } else {
                auto row = result[0];
                auto user = Json([
                    "id": row["id"].get!int,
                    "name": row["name"].get!string,
                    "email": row["email"].get!string
                ]);
                scope.response.writeJson(user);
            }
        } catch (Exception e) {
            scope.response.status = HTTPStatus.internalServerError;
            scope.response.write("Error getting user: " ~ e.msg);
        }
    });

    router.put("/users/:id", async (scope, string id) {
        scope.requireJson();
        auto json = scope.json;

        string name = json["name"].str;
        string email = json["email"].str;

        try {
            auto conn = await pool.get();
            scope.onClose(() => pool.release(conn));

            string updateQuery = format("UPDATE users SET name = '%s', email = '%s' WHERE id = %s", name, email, id);
            ulong affectedRows = await conn.execute(updateQuery);

            if (affectedRows == 0) {
                scope.response.status = HTTPStatus.notFound;
                scope.response.write("User not found");
            } else {
                scope.response.status = HTTPStatus.ok;
            }
        } catch (Exception e) {
            scope.response.status = HTTPStatus.internalServerError;
            scope.response.write("Error updating user: " ~ e.msg);
        }
    });

    router.delete("/users/:id", async (scope, string id) {
        try {
            auto conn = await pool.get();
            scope.onClose(() => pool.release(conn));

            string deleteQuery = format("DELETE FROM users WHERE id = %s", id);
            ulong affectedRows = await conn.execute(deleteQuery);

            if (affectedRows == 0) {
                scope.response.status = HTTPStatus.notFound;
                scope.response.write("User not found");
            } else {
                scope.response.status = HTTPStatus.ok;
            }
        } catch (Exception e) {
            scope.response.status = HTTPStatus.internalServerError;
            scope.response.write("Error deleting user: " ~ e.msg);
        }
    });

    auto settingsVibe = new HTTPServerSettings;
    settingsVibe.port = to!ushort(getenv("APP_PORT"));

    listenHTTP(settingsVibe, router);

    writeln("Server listening on port ", settingsVibe.port);

    runApplication();
}
