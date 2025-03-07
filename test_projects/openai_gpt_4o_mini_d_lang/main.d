
import std.stdio;
import std.json;
import vibe.vulcan;
import vibe.db.postgres;

struct User {
    int id;
    string name;
    string email;
}

auto db = PostgresConnection("host=host.docker.internal port=5432 user=testuser dbname=complang password=" ~ getenv("PGPASSWORD"));

void main() {
    // Start the API server
    auto settings = new HTTPServerSettings;
    settings.port = 8080;
    listenHTTP(settings, app);
    runApplication();
}

// Define the API routes
void app(HTTPServerRequest req, HTTPServerResponse res) {
    if (req.path == "/users" && req.method == HTTPMethod.POST) {
        createUser(req, res);
    } else if (req.path == "/users" && req.method == HTTPMethod.GET) {
        getUsers(res);
    } else if (req.path.startsWith("/users/")) {
        auto id = to!int(req.path[7 .. $]);
        if (req.method == HTTPMethod.GET) {
            getUser(id, res);
        } else if (req.method == HTTPMethod.PUT) {
            updateUser(id, req, res);
        } else if (req.method == HTTPMethod.DELETE) {
            deleteUser(id, res);
        }
    } else {
        res.status = HTTPStatus.NOT_FOUND;
    }
}

void createUser(HTTPServerRequest req, HTTPServerResponse res) {
    auto data = parseJSON(req.body);
    string name = data["name"];
    string email = data["email"];

    auto result = db.exec("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id", name, email);
    res.status = HTTPStatus.CREATED;
    res.json(JSONValue("id", result[0][0], "name", name, "email", email));
}

void getUsers(HTTPServerResponse res) {
    auto result = db.query("SELECT * FROM users");
    auto users = array();
    foreach (row; result) {
        users ~= User(row[0], row[1], row[2]);
    }
    res.json(users);
}

void getUser(int id, HTTPServerResponse res) {
    auto result = db.query("SELECT * FROM users WHERE id = $1", id);
    if (result.length == 0) {
        res.status = HTTPStatus.NOT_FOUND;
        return;
    }
    auto row = result[0];
    res.json(User(row[0], row[1], row[2]));
}

void updateUser(int id, HTTPServerRequest req, HTTPServerResponse res) {
    auto data = parseJSON(req.body);
    string name = data["name"];
    string email = data["email"];

    auto result = db.exec("UPDATE users SET name = $1, email = $2 WHERE id = $3", name, email, id);
    if (result.numUpdated == 0) {
        res.status = HTTPStatus.NOT_FOUND;
        return;
    }
    res.status = HTTPStatus.NO_CONTENT;
}

void deleteUser(int id, HTTPServerResponse res) {
    auto result = db.exec("DELETE FROM users WHERE id = $1", id);
    if (result.numDeleted == 0) {
        res.status = HTTPStatus.NOT_FOUND;
        return;
    }
    res.status = HTTPStatus.NO_CONTENT;
}
