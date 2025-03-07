
use v6;
use JSON::Fast;
use Cro::HTTP;
use Cro::HTTP::Router;

# Database connection
my $db-url = "postgres://testuser:$*ENV<PGPASSWORD>\@host.docker.internal:5432/complang";
my $db = DBI.connect($db-url);

# Create the API Router
my $router = Cro::HTTP::Router.new;

# POST /users - Create a new user
$router.add-post('/users', -> $request {
    my $data = from-json $request.body;
    my $sth = $db.prepare('INSERT INTO users (name, email) VALUES (?, ?) RETURNING id');
    $sth.execute($data<name>, $data<email>);
    my $id = $sth.fetchrow_array; 
    $request.respond(201, to-json { id => $id, name => $data<name>, email => $data<email> });
});

# GET /users - Get a list of users
$router.add-get('/users', -> $request {
    my $users = $db.query('SELECT id, name, email FROM users').map(*.hash);
    $request.respond(200, to-json $users);
});

# GET /users/{id} - Get a single user
$router.add-get('/users/{id}', -> $request {
    my $id = $request.params<{id}>;
    my $user = $db.query('SELECT id, name, email FROM users WHERE id = ?', $id).map(*.hash).first;
    if $user {
        $request.respond(200, to-json $user);
    } else {
        $request.respond(404, 'User not found');
    }
});

# PUT /users/{id} - Update a user
$router.add-put('/users/{id}', -> $request {
    my $id = $request.params<{id}>;
    my $data = from-json $request.body;
    my $sth = $db.prepare('UPDATE users SET name = ?, email = ? WHERE id = ?');
    my $rows = $sth.execute($data<name>, $data<email>, $id);
    if $rows {
        $request.respond(200, 'User updated');
    } else {
        $request.respond(404, 'User not found');
    }
});

# DELETE /users/{id} - Delete a user
$router.add-delete('/users/{id}', -> $request {
    my $id = $request.params<{id}>;
    my $sth = $db.prepare('DELETE FROM users WHERE id = ?');
    my $rows = $sth.execute($id);
    if $rows {
        $request.respond(204, 'User deleted');
    } else {
        $request.respond(404, 'User not found');
    }
});

# Start the Cro HTTP server
my $app = Cro::HTTP::App.new($router);
app.start(8080);
