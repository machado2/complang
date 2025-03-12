
use Cro::HTTP::Router;
use Cro::HTTP::Server;
use Cro::HTTP::Status;
use JSON::Fast;
use DB::Pg;
use Env::PGPASSWORD;

# Database configuration
my $db-host     = 'host.docker.internal';
my $db-port     = 5432;
my $db-name     = 'complang';
my $db-user     = 'testuser';
my $db-password = $?PGPASSWORD;

# Connect to the database
my $db = DB::Pg.new(
    host     => $db-host,
    port     => $db-port,
    dbname   => $db-name,
    user     => $db-user,
    password => $db-password
);

# Helper function to execute SQL queries
sub execute-query($sql, *@params) {
    my $sth = $db.prepare($sql);
    $sth.execute(@params);
    return $sth;
}

# Create table (if not exists) during initialization.
sub create-table() {
    my $sql = q:to/END/;
        CREATE TABLE IF NOT EXISTS users (
            id SERIAL PRIMARY KEY,
            name TEXT NOT NULL,
            email TEXT NOT NULL
        );
    END
    execute-query($sql);
}

create-table();

# Route handler for creating a user (POST /users)
sub create-user($ctx) {
    my $body = $ctx.request.body;
    my $data = from-json($body);

    my $name  = $data<name>;
    my $email = $data<email>;

    # Validate data
    if !$name || !$email {
        $ctx.response.status = Status::BadRequest;
        $ctx.response.body   = to-json({ message => "Missing name or email" });
        return;
    }

    my $sth = execute-query(
        'INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email',
        $name, $email
    );

    my $result = $sth.fetchall;
    $sth.finish;

    if $result.elems > 0 {
        $ctx.response.status = Status::Created;
        $ctx.response.body   = to-json($result[0]);
    } else {
        $ctx.response.status = Status::InternalServerError;
        $ctx.response.body   = to-json({ message => "Failed to create user" });
    }
}

# Route handler for getting all users (GET /users)
sub get-all-users($ctx) {
    my $sth = execute-query('SELECT id, name, email FROM users');
    my @users = $sth.fetchall;
    $sth.finish;
    $ctx.response.status = Status::OK;
    $ctx.response.body   = to-json(@users);
}

# Route handler for getting a single user (GET /users/{id})
sub get-user($ctx, $id) {
    my $sth = execute-query('SELECT id, name, email FROM users WHERE id = ?', $id);
    my @users = $sth.fetchall;
    $sth.finish;

    if @users.elems > 0 {
        $ctx.response.status = Status::OK;
        $ctx.response.body   = to-json(@users[0]);
    } else {
        $ctx.response.status = Status::NotFound;
        $ctx.response.body   = to-json({ message => "User not found" });
    }
}

# Route handler for updating a user (PUT /users/{id})
sub update-user($ctx, $id) {
    my $body = $ctx.request.body;
    my $data = from-json($body);

    my $name  = $data<name>;
    my $email = $data<email>;

    # Validate data
    if !$name || !$email {
        $ctx.response.status = Status::BadRequest;
        $ctx.response.body   = to-json({ message => "Missing name or email" });
        return;
    }

    my $sth = execute-query(
        'UPDATE users SET name = ?, email = ? WHERE id = ?',
        $name, $email, $id
    );

    my $rows-updated = $sth.rows;
    $sth.finish;

    if $rows-updated > 0 {
        $ctx.response.status = Status::NoContent; # Or Status::OK
    } else {
        $ctx.response.status = Status::NotFound;
        $ctx.response.body   = to-json({ message => "User not found" });
    }
}

# Route handler for deleting a user (DELETE /users/{id})
sub delete-user($ctx, $id) {
    my $sth = execute-query('DELETE FROM users WHERE id = ?', $id);
    my $rows-deleted = $sth.rows;
    $sth.finish;

    if $rows-deleted > 0 {
        $ctx.response.status = Status::NoContent; # Or Status::OK
    } else {
        $ctx.response.status = Status::NotFound;
        $ctx.response.body   = to-json({ message => "User not found" });
    }
}

# Define routes
my $router = route {
    get '/users'                      => &get-all-users;
    post '/users'                     => &create-user;
    get '/users/:id'                 => &get-user;
    put '/users/:id'                 => &update-user;
    delete '/users/:id'              => &delete-user;
};

# Start the server
sub main() {
    my $server = Cro::HTTP::Server.new(
        http => { port => 8080 },
        application => $router
    );
    $server.start;

    react {
        whenever signal(SIGINT) {
            $server.stop;
            exit;
        }
    }
}

main;
