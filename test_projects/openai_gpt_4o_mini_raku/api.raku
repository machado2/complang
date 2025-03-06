use v6;
use Cro;
use DBI;

# Database connection information
my $db-name = 'complang';
my $db-host = 'host.docker.internal';
my $db-port = 5432;
my $db-user = 'testuser';
my $db-password = ENV<'PGPASSWORD'>;

# Connect to the PostgreSQL database
my $dbh = DBI.connect('dbi:Pg:dbname=' ~ $db-name ~ ';host=' ~ $db-host ~ ';port=' ~ $db-port, $db-user, $db-password);

# Function to create a user
sub create-user($name, $email) {
    my $stmt = 'INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email';
    return $dbh.prepare($stmt).execute($name, $email).map({
        { id => $_[0], name => $_[1], email => $_[2] }
    })[0];
}

# Function to fetch all users
sub fetch-users() {
    my $stmt = 'SELECT * FROM users';
    return $dbh.prepare($stmt).execute.map({
        { id => $_[0], name => $_[1], email => $_[2] }
    });
}

# Function to fetch a single user
sub fetch-user($id) {
    my $stmt = 'SELECT * FROM users WHERE id = ?';
    return $dbh.prepare($stmt).execute($id).map({
        { id => $_[0], name => $_[1], email => $_[2] }
    })[0];
}

# Function to update a user
sub update-user($id, $name, $email) {
    my $stmt = 'UPDATE users SET name = ?, email = ? WHERE id = ?';
    return $dbh.prepare($stmt).execute($name, $email, $id);
}

# Function to delete a user
sub delete-user($id) {
    my $stmt = 'DELETE FROM users WHERE id = ?';
    return $dbh.prepare($stmt).execute($id);
}

# Start the Cro application
my app = Cro::HTTP::App.new;

app.route('POST', '/users', -> ($request) {
    my $data = $request.body;   # get the JSON data
    my $user = create-user($data.name, $data.email);
    return $request.respond(201, $user);
});

app.route('GET', '/users', -> ($request) {
    my @users = fetch-users();
    return $request.respond(200, @users);
});

app.route('GET', '/users/{id}', -> ($request, $id) {
    my $user = fetch-user($id);
    return $user ?? $request.respond(200, $user) !! $request.respond(404, 'User not found');
});

app.route('PUT', '/users/{id}', -> ($request, $id) {
    my $data = $request.body;
    if fetch-user($id) {
        update-user($id, $data.name, $data.email);
        return $request.respond(200);
    }
    return $request.respond(404, 'User not found');
});

app.route('DELETE', '/users/{id}', -> ($request, $id) {
    if fetch-user($id) {
        delete-user($id);
        return $request.respond(204);
    }
    return $request.respond(404, 'User not found');
});

app.start(port => 8080);