
use Cro::HTTP::Router;
use Cro::HTTP::Server;
use JSON::Fast;
use DBIish;
use Env;

# Database configuration
my $host     = 'host.docker.internal';
my $port     = 5432;
my $database = 'test_google_gemini_2_0_flash_001_raku';
my $user     = 'postgres';
my $password = env('PGPASSWORD') // ''; # Get password from environment variable
my $dsn      = "dbi:Pg:database=$database;host=$host;port=$port";

# Database connection
my $dbh = DBIish.connect: $dsn, $user, $password, { RaiseError => 1 };

# Check if the users table exists; create it if not
sub create_users_table() {
    my $tables = $dbh.selectall_arrayref("SELECT tablename FROM pg_catalog.pg_tables WHERE schemaname != 'pg_catalog' AND schemaname != 'information_schema'");
    my @tables = $tables.map({ $_[0] });
    unless (@tables.grep('users')) {
        $dbh.do("
            CREATE TABLE users (
                id SERIAL PRIMARY KEY,
                name TEXT,
                email TEXT
            )
        ");
    }
}

create_users_table();

# Route definitions
my $router = Cro::HTTP::Router.new;

$router.route: '/users',
    method => 'POST',
    callback => sub ($ctx) {
        my $body = $ctx.request.body.decode;
        my $data = from-json($body);
        my $name  = $data<name>;
        my $email = $data<email>;

        my $sth = $dbh.prepare("INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email");
        $sth.execute($name, $email);
        my $user = $sth.fetchrow_hashref;
        $ctx.response.status = 201;
        $ctx.response.body = to-json($user);
        $ctx.response.content-type = 'application/json';
    };

$router.route: '/users',
    method => 'GET',
    callback => sub ($ctx) {
        my $sth = $dbh.prepare("SELECT id, name, email FROM users");
        $sth.execute;
        my @users = $sth.fetchall_hashref;
        $ctx.response.status = 200;
        $ctx.response.body = to-json(@users);
        $ctx.response.content-type = 'application/json';
    };

$router.route: '/users/{id}',
    method => 'GET',
    callback => sub ($ctx) {
        my $id = $ctx.route_params<id>;
        my $sth = $dbh.prepare("SELECT id, name, email FROM users WHERE id = ?");
        $sth.execute($id);
        my $user = $sth.fetchrow_hashref;
        if $user {
            $ctx.response.status = 200;
            $ctx.response.body = to-json($user);
            $ctx.response.content-type = 'application/json';
        } else {
            $ctx.response.status = 404;
            $ctx.response.body = to-json({ message => "User not found" }); # Correct the status code
            $ctx.response.content-type = 'application/json';
        }
    };

$router.route: '/users/{id}',
    method => 'PUT',
    callback => sub ($ctx) {
        my $id = $ctx.route_params<id>;
        my $body = $ctx.request.body.decode;
        my $data = from-json($body);
        my $name  = $data<name>;
        my $email = $data<email>;

        my $sth = $dbh.prepare("UPDATE users SET name = ?, email = ? WHERE id = ?");
        $sth.execute($name, $email, $id);
        if $dbh.rows {
            $ctx.response.status = 204;
        } else {
            $ctx.response.status = 404;
            $ctx.response.body = to-json({ message => "User not found" });
            $ctx.response.content-type = 'application/json';
        }
    };

$router.route: '/users/{id}',
    method => 'DELETE',
    callback => sub ($ctx) {
        my $id = $ctx.route_params<id>;
        my $sth = $dbh.prepare("DELETE FROM users WHERE id = ?");
        $sth.execute($id);
        if $dbh.rows {
            $ctx.response.status = 204;
        } else {
            $ctx.response.status = 404;
            $ctx.response.body = to-json({ message => "User not found" });
            $ctx.response.content-type = 'application/json';
        }
    };

# Start the server
my $port = 8080;
Cro::HTTP::Server.new(
    http => <1.1>,
    host => '0.0.0.0',
    port => $port,
    router => $router
).start;
