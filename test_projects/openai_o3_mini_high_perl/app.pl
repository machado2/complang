#!/usr/bin/env perl
use strict;
use warnings;
use Mojolicious::Lite;
use DBI;

# Retrieve the PostgreSQL password from environment variable
my $pg_password = $ENV{"PGPASSWORD"} // "";

# Connect to the PostgreSQL database
my $dsn = "dbi:Pg:dbname=complang;host=host.docker.internal;port=5432";
my $dbh = DBI->connect($dsn, "testuser", $pg_password, { RaiseError => 1, AutoCommit => 1 })
    or die "Could not connect to database: $DBI::errstr";

# Make the database handle available as a helper
helper db => sub { return $dbh };

# POST /users: Create a new user
post "/users" => sub {
    my $c = shift;
    my $data = $c->req->json;
    unless ($data && $data->{name} && $data->{email}) {
        return $c->render(json => { error => "Missing name or email" }, status => 400);
    }
    my $name  = $data->{name};
    my $email = $data->{email};
    my $sth = $c->app->db->prepare("INSERT INTO users (name, email) VALUES (?, ?) RETURNING id");
    $sth->execute($name, $email);
    my ($id) = $sth->fetchrow_array;
    $c->render(json => { id => $id, name => $name, email => $email }, status => 201);
};

# GET /users: Get list of all users
get "/users" => sub {
    my $c = shift;
    my $sth = $c->app->db->prepare("SELECT id, name, email FROM users");
    $sth->execute();
    my @users;
    while (my $row = $sth->fetchrow_hashref) {
        push @users, $row;
    }
    $c->render(json => \@users, status => 200);
};

# GET /users/:id: Get a single user by id
get "/users/:id" => sub {
    my $c  = shift;
    my $id = $c->param("id");
    my $sth = $c->app->db->prepare("SELECT id, name, email FROM users WHERE id = ?");
    $sth->execute($id);
    if (my $user = $sth->fetchrow_hashref) {
        $c->render(json => $user, status => 200);
    } else {
        $c->render(json => { error => "User not found" }, status => 404);
    }
};

# PUT /users/:id: Update a user
put "/users/:id" => sub {
    my $c  = shift;
    my $id = $c->param("id");
    my $data = $c->req->json;
    unless ($data && $data->{name} && $data->{email}) {
        return $c->render(json => { error => "Missing name or email" }, status => 400);
    }
    my $name  = $data->{name};
    my $email = $data->{email};
    my $sth = $c->app->db->prepare("UPDATE users SET name = ?, email = ? WHERE id = ?");
    my $rows = $sth->execute($name, $email, $id);
    if ($rows && $rows > 0) {
        $c->render(json => { success => 1 }, status => 200);
    } else {
        $c->render(json => { error => "User not found" }, status => 404);
    }
};

# DELETE /users/:id: Delete a user
del "/users/:id" => sub {
    my $c  = shift;
    my $id = $c->param("id");
    my $sth = $c->app->db->prepare("DELETE FROM users WHERE id = ?");
    my $rows = $sth->execute($id);
    if ($rows && $rows > 0) {
        $c->render(json => { success => 1 }, status => 200);
    } else {
        $c->render(json => { error => "User not found" }, status => 404);
    }
};

# Start the Mojolicious daemon listening on port 8080
app->start("daemon", "-l", "http://*:8080");
