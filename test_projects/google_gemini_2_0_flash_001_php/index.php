<?php

// Database configuration
$host = "host.docker.internal";
$port = 5432;
$dbname = "complang";
$user = "testuser";
$password = getenv("PGPASSWORD"); // Get password from environment variable

// Data source name
$dsn = "pgsql:host=$host;port=$port;dbname=$dbname;";

// Function to connect to the database
function connectDB() {
    global $dsn, $user, $password;
    try {
        $pdo = new PDO($dsn, $user, $password);
        $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
        return $pdo;
    } catch (PDOException $e) {
        http_response_code(500);
        echo json_encode(["error" => "Database connection failed: " . $e->getMessage()]);
        exit;
    }
}

// Get the request method and URI
$method = $_SERVER["REQUEST_METHOD"];
$uri = $_SERVER["REQUEST_URI"];

// Basic routing
if (strpos($uri, "/users") === 0) {
    handleUsersRequest($method, $uri);
} else {
    http_response_code(404);
    echo json_encode(["error" => "Endpoint not found"]);
}

function handleUsersRequest($method, $uri) {
    $pdo = connectDB();

    switch ($method) {
        case "POST":
            createUser($pdo);
            break;
        case "GET":
            if (preg_match("/\/users\/(\d+)/", $uri, $matches)) {
                $id = $matches[1];
                getUser($pdo, $id);
            } else {
                getAllUsers($pdo);
            }
            break;
        case "PUT":
            if (preg_match("/\/users\/(\d+)/", $uri, $matches)) {
                $id = $matches[1];
                updateUser($pdo, $id);
            } else {
                http_response_code(400);
                echo json_encode(["error" => "Invalid request"]);
            }
            break;
        case "DELETE":
            if (preg_match("/\/users\/(\d+)/", $uri, $matches)) {
                $id = $matches[1];
                deleteUser($pdo, $id);
            } else {
                http_response_code(400);
                echo json_encode(["error" => "Invalid request"]);
            }
            break;
        default:
            http_response_code(405);
            echo json_encode(["error" => "Method not allowed"]);
    }
}

function createUser($pdo) {
    $body = json_decode(file_get_contents("php://input"), true);

    if (!isset($body["name"]) || !isset($body["email"])) {
        http_response_code(400);
        echo json_encode(["error" => "Name and email are required"]);
        return;
    }

    $name = $body["name"];
    $email = $body["email"];

    try {
        $stmt = $pdo->prepare("INSERT INTO users (name, email) VALUES (?, ?) RETURNING id");
        $stmt->execute([$name, $email]);
        $id = $stmt->fetchColumn();

        http_response_code(201);
        echo json_encode(["id" => $id, "name" => $name, "email" => $email]);
    } catch (PDOException $e) {
        http_response_code(500);
        echo json_encode(["error" => "Failed to create user: " . $e->getMessage()]);
    }
}

function getAllUsers($pdo) {
    try {
        $stmt = $pdo->query("SELECT id, name, email FROM users");
        $users = $stmt->fetchAll(PDO::FETCH_ASSOC);

        http_response_code(200);
        echo json_encode($users);
    } catch (PDOException $e) {
        http_response_code(500);
        echo json_encode(["error" => "Failed to get users: " . $e->getMessage()]);
    }
}

function getUser($pdo, $id) {
    try {
        $stmt = $pdo->prepare("SELECT id, name, email FROM users WHERE id = ?");
        $stmt->execute([$id]);
        $user = $stmt->fetch(PDO::FETCH_ASSOC);

        if ($user) {
            http_response_code(200);
            echo json_encode($user);
        } else {
            http_response_code(404);
            echo json_encode(["error" => "User not found"]);
        }
    } catch (PDOException $e) {
        http_response_code(500);
        echo json_encode(["error" => "Failed to get user: " . $e->getMessage()]);
    }
}

function updateUser($pdo, $id) {
    $body = json_decode(file_get_contents("php://input"), true);

    if (!isset($body["name"]) || !isset($body["email"])) {
        http_response_code(400);
        echo json_encode(["error" => "Name and email are required"]);
        return;
    }

    $name = $body["name"];
    $email = $body["email"];

    try {
        $stmt = $pdo->prepare("UPDATE users SET name = ?, email = ? WHERE id = ?");
        $stmt->execute([$name, $email, $id]);

        if ($stmt->rowCount() > 0) {
            http_response_code(204); //No Content
        } else {
            http_response_code(404);
            echo json_encode(["error" => "User not found"]);
        }
    } catch (PDOException $e) {
        http_response_code(500);
        echo json_encode(["error" => "Failed to update user: " . $e->getMessage()]);
    }
}

function deleteUser($pdo, $id) {
    try {
        $stmt = $pdo->prepare("DELETE FROM users WHERE id = ?");
        $stmt->execute([$id]);

        if ($stmt->rowCount() > 0) {
            http_response_code(204); // No content
        } else {
            http_response_code(404);
            echo json_encode(["error" => "User not found"]);
        }
    } catch (PDOException $e) {
        http_response_code(500);
        echo json_encode(["error" => "Failed to delete user: " . $e->getMessage()]);
    }
}
?>