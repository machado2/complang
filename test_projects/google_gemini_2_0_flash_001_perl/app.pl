
use Mojolicious::Lite;
use DBI;
use JSON;

# Database configuration
my $host     = 'host.docker.internal';
my $port     = 5432;
my $database = 'complang';
my $user     = 'testuser';
my $password = $ENV{PGPASSWORD}; # Read password from environment variable

# Construct the connection string
my $dsn = "dbi:Pg:dbname=$database;host=$host;port=$port";

# Helper function to connect to the database
sub db_connect {
    my $dbh = DBI->connect($dsn, $user, $password, { RaiseError => 1, AutoCommit => 0 });
    return $dbh;
}

# before_route hook to handle common tasks (e.g., JSON parsing)
hook before_route => sub {
    my $c = shift;
    if ($c->req->is_json) {
        my $json = decode_json $c->req->body;
        $c->stash(json => $json);
    }
};


# GET /users: Returns a list of all users
get '/users' => sub {
    my $c = shift;
    my $dbh = db_connect();
    my $sth = $dbh->prepare('SELECT id, name, email FROM users');
    $sth->execute();

    my @users;
    while (my $row = $sth->fetchrow_hashref()) {
        push @users, $row;
    }

    $sth->finish();
    $dbh->disconnect();

    $c->render(json => \@users);
};

# GET /users/{id}: Returns a single user
get '/users/:id' => sub {
    my $c = shift;
    my $id = $c->param('id');

    my $dbh = db_connect();
    my $sth = $dbh->prepare('SELECT id, name, email FROM users WHERE id = ?');
    $sth->execute($id);

    my $user = $sth->fetchrow_hashref();

    $sth->finish();
    $dbh->disconnect();

    if ($user) {
        $c->render(json => $user);
    } else {
        $c->render(status => 404, json => { error => 'User not found' });
    }
};

# POST /users: Creates a new user
post '/users' => sub {
    my $c = shift;
    my $json = $c->stash('json');

    my $name  = $json->{name};
    my $email = $json->{email};

    my $dbh = db_connect();
    my $sth = $dbh->prepare('INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email');
    $sth->execute($name, $email);

    my $new_user = $sth->fetchrow_hashref();
    $dbh->commit();

    $sth->finish();
    $dbh->disconnect();

    $c->render(status => 201, json => $new_user);
};

# PUT /users/{id}: Updates an existing user
put '/users/:id' => sub {
    my $c = shift;
    my $id   = $c->param('id');
    my $json = $c->stash('json');

    my $name  = $json->{name};
    my $email = $json->{email};

    my $dbh = db_connect();
    my $sth = $dbh->prepare('UPDATE users SET name = ?, email = ? WHERE id = ?');
    $sth->execute($name, $email, $id);

    my $rows_affected = $sth->rows;
    $dbh->commit();

    $sth->finish();
    $dbh->disconnect();

    if ($rows_affected > 0) {
        $c->render(status => 204);
    } else {
        $c->render(status => 404, json => { error => 'User not found' });
    }
};

# DELETE /users/{id}: Deletes an existing user
delete '/users/:id' => sub {
    my $c = shift;
    my $id = $c->param('id');

    my $dbh = db_connect();
    my $sth = $dbh->prepare('DELETE FROM users WHERE id = ?');
    $sth->execute($id);

    my $rows_affected = $sth->rows;
    $dbh->commit();

    $sth->finish();
    $dbh->disconnect();

    if ($rows_affected > 0) {
        $c->render(status => 204);
    } else {
        $c->render(status => 404, json => { error => 'User not found' });
    }
};

app->start;
