use Cro::HTTP::Router;
use Cro::HTTP::Server;
use JSON::Fast;
use DBIish;
use Env::PGPASSWORD;

# Database connection details
my $host     = 'host.docker.internal';
my $port     = 5432;
my $database = 'complang';
my $user     = 'testuser';
my $password = $ENV{PGPASSWORD} // die "PGPASSWORD environment variable not set";

# Database connection string
my $dsn = "dbi:Pg:dbname=$database;host=$host;port=$port";

# Function to connect to the database
sub connect-db {
    return DBIish.connect: $dsn, $user, $password, { RaiseError => 1, PrintError => 0 };
}


# Route definitions
my $router = Cro::HTTP::Router.new(
    route {
        get {
            header "Content-Type" => "application/json";
            path "/users" => {
                my $dbh = connect-db();
                my $sth = $dbh.prepare("SELECT id, name, email FROM users");
                $sth.execute();
                my @users;
                while my $row = $sth.fetchrow_hashref() {
                    push @users, {
                        id    => $row<id>.Int,
                        name  => $row<name>.Str,
                        email => $row<email>.Str,
                    };
                }
                $sth.finish();
                $dbh.disconnect();
                content encode-json @users;
            }
            path "/users/:id" => {
                my $id = $/.captures[0];
                my $dbh = connect-db();
                my $sth = $dbh.prepare("SELECT id, name, email FROM users WHERE id = ?");
                $sth.execute($id);
                if my $row = $sth.fetchrow_hashref() {
                    $sth.finish();
                    $dbh.disconnect();
                    content encode-json {
                        id    => $row<id>.Int,
                        name  => $row<name>.Str,
                        email => $row<email>.Str,
                    };
                } else {
                    $sth.finish();
                    $dbh.disconnect();
                    status 404;
                    content encode-json { message => "User not found" };
                }
            }
        }
        post {
            header "Content-Type" => "application/json";
            path "/users" => {
                my $json = decode-json request.body.decode;
                my $name  = $json<name>;
                my $email = $json<email>;

                unless (defined $name && defined $email) {
                    status 400;
                    content encode-json { message => "Name and email are required" };
                    next;
                }
                my $dbh = connect-db();
                my $sth = $dbh.prepare("INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email");
                $sth.execute($name, $email);
                my $row = $sth.fetchrow_hashref();
                $sth.finish();
                $dbh.disconnect();
                status 201;
                content encode-json {
                    id    => $row<id>.Int,
                    name  => $row<name>.Str,
                    email => $row<email>.Str,
                };
            }
        }
        put {
            header "Content-Type" => "application/json";
            path "/users/:id" => {
                my $id   = $/.captures[0];
                my $json = decode-json request.body.decode;
                my $name  = $json<name>;
                my $email = $json<email>;

                unless (defined $name && defined $email) {
                    status 400;
                    content encode-json { message => "Name and email are required" };
                    next;
                }
                my $dbh = connect-db();
                my $sth = $dbh.prepare("UPDATE users SET name = ?, email = ? WHERE id = ? RETURNING id, name, email");
                $sth.execute($name, $email, $id);
                if (my $row = $sth.fetchrow_hashref()) {
                    $sth.finish();
                    $dbh.disconnect();
                    status 200;
                } else {
                    $sth.finish();
                    $dbh.disconnect();
                    status 404;
                    content encode-json { message => "User not found" };
                }
            }
        }
        delete {
            header "Content-Type" => "application/json";
            path "/users/:id" => {
                my $id = $/.captures[0];
                my $dbh = connect-db();
                my $sth = $dbh.prepare("DELETE FROM users WHERE id = ? RETURNING id");
                $sth.execute($id);
                if ($sth.rows) {
                  $sth.finish();
                  $dbh.disconnect();
                  status 200;
                }
                else {
                    $sth.finish();
                    $dbh.disconnect();
                    status 404;
                    content encode-json { message => "User not found" };
                }
            }
        }
    }
);

# Start the server
Cro::HTTP::Server.new(
    http => { port => 8080 },
    application => $router
).start;

