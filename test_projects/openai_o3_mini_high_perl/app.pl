#!/usr/bin/env perl
use strict;
use warnings;
use Mojolicious::Lite;
use DBI;

# Ensure the PGPASSWORD environment variable is set
my $db_password = $ENV{'PGPASSWORD'} // die "PGPASSWORD environment variable not set";

# Database connection configuration
my $dsn = "DBI:Pg:dbname=complang;host=host.docker.internal;port=5432";
my $db_username = "testuser";

# Create a persistent database handle
my $dbh = DBI->connect($dsn, $db_username, $db_password, {
    RaiseError => 1,
    AutoCommit => 1,
}) or die $DBI::errstr;

# Helper to access the DB handle from routes
helper db => sub { return $dbh };

# POST /users: Create a new user
post '/users' => sub {
    my $c = shift;
    my $data = $c->req->json;
    # Validate input
    unless ($data && defined $data->{name} && defined $data->{email}) {
        return $c->render(json => { error => 'Invalid input' }, status => 400);
    }
    my $name  = $data->{name};
    my $email = $data->{email};
    # Insert the user and return the new id using PostgreSQL RETURNING clause
    my $sth = $dbh->prepare("INSERT INTO users (name, email) VALUES (?, ?) RETURNING id");
    $sth->execute($name, $email);
    my ($id) = $sth->fetchrow_array;
    $sth->finish;
    return $c->render(json => { id => $id, name => $name, email => $email }, status => 201);
};

# GET /users: List all users
get '/users' => sub {
    my $c = shift;
    my $sth = $dbh->prepare("SELECT id, name, email FROM users ORDER BY id");
    $sth->execute();
    my @users;
    while (my $row = $sth->fetchrow_hashref) {
        push @users, $row;
    }
    $sth->finish;
    return $c->render(json => \@users, status => 200);
};

# GET /users/:id: Get a user by id
get '/users/:id' => sub {
    my $c = shift;
    my $id = $c->stash('id');
    my $sth = $dbh->prepare("SELECT id, name, email FROM users WHERE id = ?");
    $sth->execute($id);
    my $user = $sth->fetchrow_hashref;
    $sth->finish;
    if ($user) {
        return $c->render(json => $user, status => 200);
    } else {
        return $c->render(json => { error => "User not found" }, status => 404);
    }
};

# PUT /users/:id: Update a user by id
put '/users/:id' => sub {
    my $c = shift;
    my $id = $c->stash('id');
    my $data = $c->req->json;
    # Validate input
    unless ($data && defined $data->{name} && defined $data->{email}) {
        return $c->render(json => { error => 'Invalid input' }, status => 400);
    }
    my $name  = $data->{name};
    my $email = $data->{email};
    my $sth = $dbh->prepare("UPDATE users SET name = ?, email = ? WHERE id = ?");
    my $rows = $sth->execute($name, $email, $id);
    $sth->finish;
    if ($rows && $rows > 0) {
        return $c->render(json => { id => int($id), name => $name, email => $email }, status => 200);
    } else {
        return $c->render(json => { error => "User not found" }, status => 404);
    }
};

# DELETE /users/:id: Delete a user by id
del '/users/:id' => sub {
    my $c = shift;
    my $id = $c->stash('id');
    my $sth = $dbh->prepare("DELETE FROM users WHERE id = ?");
    my $rows = $sth->execute($id);
    $sth->finish;
    if ($rows && $rows > 0) {
        # Return 204 No Content on success
        return $c->render(status => 204, text => '');
    } else {
        return $c->render(json => { error => "User not found" }, status => 404);
    }
};

# Start the server on port 8080
app->start('daemon', '-l', 'http://*:8080');
