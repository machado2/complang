#!/usr/bin/env raku

use v6;
use Mojolicious::Lite;
use DBIish;
use JSON::Fast;
use Env;

# Database configuration
my $host     = "host.docker.internal";
my $port     = 5432;
my $database = "complang";
my $user     = "testuser";
my $password = $*ENV{"PGPASSWORD"} // ""; # Get password from environment variable

# Connect to the database
my $dbh = DBIish.connect("Pg",
    host     => $host,
    port     => $port,
    database => $database,
    user     => $user,
    password => $password,
);

# Check connection
die "Could not connect to database: $dbh.errstr" unless $dbh;

# Helper function to convert result set to JSON
sub result_to_json ($result) {
    my @rows;
    while ($result.next()) {
        push @rows, $result.row;
    }
    return to-json(@rows);
}

# GET /users: Returns a list of all users
get 
'/users' => sub {
    my $c = shift;
    my $sth = $dbh.prepare("SELECT id, name, email FROM users");
    $sth.execute;
    my $json = result_to_json($sth);
    $sth.finish;
    $c.render(json => from-json($json));
};

# GET /users/{id}: Returns a single user
get '/users/:id' => sub {
    my $c = shift;
    my $id = $c.param('id');
    my $sth = $dbh.prepare("SELECT id, name, email FROM users WHERE id = ?");
    $sth.execute($id);
    my $result = $sth.allrows;
    $sth.finish;

    if ($result.elems) {
        $c.render(json => $result[0]);
    } else {
        $c->render(status => 404, text => 'User not found');
    }
};

# POST /users: Creates a user
post '/users' => sub (
  my $c = shift
) {
  my $data = from-json($c.req.body);
  my $name  = $data<name>;
  my $email = $data<email>;

  unless defined $name and defined $email {
    return $c.render(status => 400, text => "Missing name or email");
  }

  my $sth = $dbh.prepare("INSERT INTO users (name, email) VALUES (?, ?) RETURNING id");
  $sth.execute($name, $email);
  my $user_id = $sth.firstrow[0];
  $sth.finish;

  my $select_sth = $dbh.prepare("SELECT id, name, email FROM users WHERE id = ?");
  $select_sth.execute($user_id);
  my $new_user = $select_sth.allrows[0];
  $select_sth.finish;

  $c.res.status(201);
  $c.render(json => $new_user);

};

# PUT /users/{id}: Updates a user
put '/users/:id' => sub {
    my $c = shift;
    my $id = $c.param('id');
    my $data = from-json($c.req.body);
    my $name  = $data<name>;
    my $email = $data<email>;

    my $sth = $dbh.prepare("UPDATE users SET name = ?, email = ? WHERE id = ?");
    $sth.execute($name, $email, $id);

    if ($sth.rows == 0) {
        $c->render(status => 404, text => 'User not found');
    } else {
        $c->res->status(204);
        $c->render(text => '');
    }
    $sth.finish;
};

# DELETE /users/{id}: Deletes a user
delete '/users/:id' => sub {
    my $c = shift;
    my $id = $c.param('id');

    my $sth = $dbh.prepare("DELETE FROM users WHERE id = ?");
    $sth.execute($id);

    if ($sth.rows == 0) {
        $c->render(status => 404, text => 'User not found');
    } else {
        $c->res->status(204);
        $c->render(text => '');
    }
    $sth.finish;
};


# Start the application
app->start;


