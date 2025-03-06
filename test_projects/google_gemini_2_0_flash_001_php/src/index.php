<?php

use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\Factory\AppFactory;

require __DIR__ . '/../vendor/autoload.php';

$app = AppFactory::create();

$app->addBodyParsingMiddleware();

$db_host = 'host.docker.internal';
$db_port = 5432;
$db_name = 'complang';
$db_user = 'testuser';
$db_pass = $_ENV['PGPASSWORD'];

$dsn = "pgsql:host=$db_host;port=$db_port;dbname=$db_name;";

function get_db(): PDO {
    global $dsn, $db_user, $db_pass;
    try {
        $pdo = new PDO($dsn, $db_user, $db_pass);
        $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
        return $pdo;
    } catch (PDOException $e) {
        throw new Exception("Database connection failed: " . $e->getMessage());
    }
}

$app->options('/{routes:.+}', function ($request, $response, $args) {
    return $response;
});

$app->add(function ($request, $handler) {
    $response = $handler->handle($request);
    return $response
            ->withHeader('Access-Control-Allow-Origin', '*')
            ->withHeader('Access-Control-Allow-Headers', 'X-Requested-With, Content-Type, Accept, Origin, Authorization')
            ->withHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, PATCH, OPTIONS');
});


$app->get('/', function (Request $request, Response $response, $args) {
    $response->getBody()->write("Hello world!");
    return $response;
});


$app->post('/users', function (Request $request, Response $response, $args) {

    $data = $request->getParsedBody();

    if (!isset($data['name']) || !isset($data['email'])) {
        $response->getBody()->write(json_encode(['error' => 'Missing name or email']));
        return $response->withStatus(400)
                         ->withHeader('Content-Type', 'application/json');
    }

    $name = $data['name'];
    $email = $data['email'];

    try {
        $pdo = get_db();

        $stmt = $pdo->prepare("INSERT INTO users (name, email) VALUES (:name, :email) RETURNING id");
        $stmt->bindParam(':name', $name);
        $stmt->bindParam(':email', $email);
        $stmt->execute();
        $user_id = $stmt->fetchColumn();


        $user = ['id' => (int)$user_id, 'name' => $name, 'email' => $email];

        $response->getBody()->write(json_encode($user));

        return $response->withStatus(201)
                         ->withHeader('Content-Type', 'application/json');

    } catch(PDOException $e) {
        $error = array ('error' => $e->getMessage());
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(500)
                         ->withHeader('Content-Type', 'application/json');
    } catch (Exception $e) {
        $response->getBody()->write(json_encode(['error' => $e->getMessage()]));
        return $response->withStatus(500)
                         ->withHeader('Content-Type', 'application/json');
    }

});


$app->get('/users', function (Request $request, Response $response, $args) {
    try {
        $pdo = get_db();
        $stmt = $pdo->query("SELECT id, name, email FROM users");
        $users = $stmt->fetchAll(PDO::FETCH_ASSOC);
        $response->getBody()->write(json_encode($users));
        return $response->withStatus(200)
                         ->withHeader('Content-Type', 'application/json');
    } catch(PDOException $e) {
        $error = array ('error' => $e->getMessage());
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(500)
                         ->withHeader('Content-Type', 'application/json');
    } catch (Exception $e) {
        $response->getBody()->write(json_encode(['error' => $e->getMessage()]));
        return $response->withStatus(500)
                         ->withHeader('Content-Type', 'application/json');
    }
});

$app->get('/users/{id}', function (Request $request, Response $response, $args) {
    $user_id = (int)$args['id'];

    try {
        $pdo = get_db();
        $stmt = $pdo->prepare("SELECT id, name, email FROM users WHERE id = :id");
        $stmt->bindParam(':id', $user_id);
        $stmt->execute();
        $user = $stmt->fetch(PDO::FETCH_ASSOC);

        if ($user) {
            $response->getBody()->write(json_encode($user));
            return $response->withStatus(200)
                             ->withHeader('Content-Type', 'application/json');
        } else {
            return $response->withStatus(404);
        }

    } catch(PDOException $e) {
        $error = array ('error' => $e->getMessage());
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(500)
                         ->withHeader('Content-Type', 'application/json');
    } catch (Exception $e) {
        $response->getBody()->write(json_encode(['error' => $e->getMessage()]));
        return $response->withStatus(500)
                         ->withHeader('Content-Type', 'application/json');
    }
});

$app->put('/users/{id}', function (Request $request, Response $response, $args) {
    $user_id = (int)$args['id'];
    $data = $request->getParsedBody();

    if (!isset($data['name']) || !isset($data['email'])) {
        $response->getBody()->write(json_encode(['error' => 'Missing name or email']));
        return $response->withStatus(400)
                         ->withHeader('Content-Type', 'application/json');
    }

    $name = $data['name'];
    $email = $data['email'];

    try {
        $pdo = get_db();
        $stmt = $pdo->prepare("UPDATE users SET name = :name, email = :email WHERE id = :id");
        $stmt->bindParam(':id', $user_id);
        $stmt->bindParam(':name', $name);
        $stmt->bindParam(':email', $email);
        $stmt->execute();

        if ($stmt->rowCount() > 0) {
            return $response->withStatus(204);
        } else {
            return $response->withStatus(404);
        }

    } catch(PDOException $e) {
        $error = array ('error' => $e->getMessage());
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(500)
                         ->withHeader('Content-Type', 'application/json');
    } catch (Exception $e) {
        $response->getBody()->write(json_encode(['error' => $e->getMessage()]));
        return $response->withStatus(500)
                         ->withHeader('Content-Type', 'application/json');
    }
});

$app->delete('/users/{id}', function (Request $request, Response $response, $args) {
    $user_id = (int)$args['id'];

    try {
        $pdo = get_db();
        $stmt = $pdo->prepare("DELETE FROM users WHERE id = :id");
        $stmt->bindParam(':id', $user_id);
        $stmt->execute();

        if ($stmt->rowCount() > 0) {
            return $response->withStatus(204);
        } else {
            return $response->withStatus(404);
        }

    } catch(PDOException $e) {
        $error = array ('error' => $e->getMessage());
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(500)
                         ->withHeader('Content-Type', 'application/json');
    } catch (Exception $e) {
        $response->getBody()->write(json_encode(['error' => $e->getMessage()]));
        return $response->withStatus(500)
                         ->withHeader('Content-Type', 'application/json');
    }
});


$app->run();