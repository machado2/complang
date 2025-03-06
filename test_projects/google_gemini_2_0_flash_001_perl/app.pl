#!/usr/bin/perl

use strict;
use warnings;
use Dancer2;
use DBI;
use JSON::XS;

# Database configuration
my $host     = 'host.docker.internal';
my $port     = 5432;
my $database = 'complang';
my $user     = 'testuser';
my $password = $ENV{PGPASSWORD};

# Database connection string
my $dsn = "dbi:Pg:dbname=$database;host=$host;port=$port";

# Route for creating a user (POST /users)
post '/users' => sub {
    my $body = request->body;
    my $data = decode_json $body;

    my $name  = $data->{name};
    my $email = $data->{email};

    unless ($name && $email) {
        status 400;
        content_type 'application/json';
        return encode_json { error => "Missing name or email" };
    }

    my $dbh = DBI->connect($dsn, $user, $password, { RaiseError => 1 });
    my $sth = $dbh->prepare("INSERT INTO users (name, email) VALUES (?, ?) RETURNING id");
    $sth->execute($name, $email);

    my $id = $sth->fetchrow_arrayref->[0];

    $sth->finish();
    $dbh->disconnect();

    status 201;
    my $user = { id => $id, name => $name, email => $email };
    content_type 'application/json';
    return encode_json $user;
};

# Route for getting all users (GET /users)
get '/users' => sub {
    my $dbh = DBI->connect($dsn, $user, $password, { RaiseError => 1 });
    my $sth = $dbh->prepare("SELECT id, name, email FROM users");
    $sth->execute();

    my @users;
    while (my $row = $sth->fetchrow_hashref()) {
        push @users, $row;
    }

    $sth->finish();
    $dbh->disconnect();

    content_type 'application/json';
    return encode_json \@users;
};

# Route for getting a single user (GET /users/:id)
get '/users/:id' => sub {
    my $id = param('id');

    my $dbh = DBI->connect($dsn, $user, $password, { RaiseError => 1 });
    my $sth = $dbh->prepare("SELECT id, name, email FROM users WHERE id = ?");
    $sth->execute($id);

    my $user = $sth->fetchrow_hashref();

    $sth->finish();
    $dbh->disconnect();

    if ($user) {
        content_type 'application/json';
        return encode_json $user;
    } else {
        status 404;
        content_type 'application/json';
        return encode_json { error => "User not found" };
    }
};

# Route for updating a user (PUT /users/:id)
put '/users/:id' => sub {
    my $id   = param('id');
    my $body = request->body;
    my $data = decode_json $body;

    my $name  = $data->{name};
    my $email = $data->{email};

    unless ($name && $email) {
        status 400;
        content_type 'application/json';
        return encode_json { error => "Missing name or email" };
    }

    my $dbh = DBI->connect($dsn, $user, $password, { RaiseError => 1 });
    my $sth = $dbh->prepare("UPDATE users SET name = ?, email = ? WHERE id = ?");
    $sth->execute($name, $email, $id);

    my $rows_updated = $sth->rows;

    $sth->finish();
    $dbh->disconnect();

    if ($rows_updated > 0) {
        status 204;
        return;
    } else {
        status 404;
        content_type 'application/json';
        return encode_json { error => "User not found" };
    }
};

# Route for deleting a user (DELETE /users/:id)
delete '/users/:id' => sub {
    my $id = param('id');

    my $dbh = DBI->connect($dsn, $user, $password, { RaiseError => 1 });
    my $sth = $dbh->prepare("DELETE FROM users WHERE id = ?");
    $sth->execute($id);

    my $rows_deleted = $sth->rows;

    $sth->finish();
    $dbh->disconnect();

    if ($rows_deleted > 0) {
        status 204;
        return;
    } else {
        status 404;
        content_type 'application/json';
        return encode_json { error => "User not found" };
    }
};

# Start the application
set port => 8080;
set serializer => 'JSON::XS';
start;
