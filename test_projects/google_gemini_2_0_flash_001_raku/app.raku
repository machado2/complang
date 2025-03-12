use Cro::HTTP;
use Cro::Route;
use JSON::Fast;
use DB::Pg;
use Env;

# Database Configuration
my $db-host     = 'host.docker.internal';
my $db-port     = 5432;
my $db-name     = 'complang';
my $db-user     = 'testuser';
my $db-password = getenv('PGPASSWORD') // die 'PGPASSWORD environment variable not set';

# Database connection string
my $conn-string = "host=$db-host port=$db-port dbname=$db-name user=$db-user password=$db-password";

# Route table
my $application = route {
    get    -> '/users'        => { getAllUsers() };
    post   -> '/users'        => { createUser() };
    get    -> '/users/:id'    => { getUser(.id) };
    put    -> '/users/:id'    => { updateUser(.id) };
    delete -> '/users/:id'    => { deleteUser(.id) };

    not-found => { content(404, 'Not found') }
};

# --- Route Handlers ---

sub getAllUsers() {
    my $db = DB::Pg.new($conn-string);
    my $users = $db.query('SELECT id, name, email FROM users');
    my @results;
    while $users.fetch() -> $row {
        push @results, {
            id    => +$row<id>,
            name  => $row<name>,
            email => $row<email>
        };
    }
    $db.close;
    content(200, to-json @results);
}

sub createUser() {
    my $body = from-json request.body;
    my $name  = $body<name>;
    my $email = $body<email>;

    unless ($name && $email) {
        content(400, 'Bad Request: Missing name or email');
        return;
    }

    my $db = DB::Pg.new($conn-string);
    my $result = $db.query("INSERT INTO users (name, email) VALUES ('$name', '$email') RETURNING id, name, email");
    $result.fetch() -> $row;
    my $user = {
        id    => +$row<id>,
        name  => $row<name>,
        email => $row<email>
    };
    $db.close;
    content(201, to-json $user);
}

sub getUser(Int:D $id) {
    my $db = DB::Pg.new($conn-string);
    my $result = $db.query("SELECT id, name, email FROM users WHERE id = $id");
    unless ($result.rows) {
        $db.close;
        content(404, 'Not Found');
        return;
    }
    $result.fetch() -> $row;
    my $user = {
        id    => +$row<id>,
        name  => $row<name>,
        email => $row<email>
    };
    $db.close;
    content(200, to-json $user);
}

sub updateUser(Int:D $id) {
    my $body = from-json request.body;
    my $name  = $body<name>;
    my $email = $body<email>;

    unless ($name && $email) {
        content(400, 'Bad Request: Missing name or email');
        return;
    }

    my $db = DB::Pg.new($conn-string);
    my $result = $db.query("UPDATE users SET name = '$name', email = '$email' WHERE id = $id");

    if ($db.affected-rows == 0) {
        $db.close;
        content(404, 'Not Found');
        return;
    }

    $db.close;
    content(204, '');
}

sub deleteUser(Int:D $id) {
    my $db = DB::Pg.new($conn-string);
    my $result = $db.query("DELETE FROM users WHERE id = $id");

    if ($db.affected-rows == 0) {
        $db.close;
        content(404, 'Not Found');
        return;
    }

    $db.close;
    content(204, '');
}

# Start the server
my $port = 8080;
my $server = Cro::HTTP::Server.new(
    port => $port,
    application => $application
);

$server.start;

react {
    whenever signal(SIGINT) {
        say "Exiting...";
        $server.stop;
        exit;
    }
}
