#!/usr/bin/env perl
use strict;
use warnings;
use Dancer2;
use DBI;
use JSON qw(encode_json decode_json);  # Importing the required functions

# Configure database connection
my $dbname = "complang";
my $host = "host.docker.internal";
my $port = 5432;
my $user = "testuser";
my $password = $ENV{"PGPASSWORD"};

my $dbh = DBI->connect("dbi:Pg:dbname=$dbname;host=$host;port=$port", $user, $password, { RaiseError => 1, AutoCommit => 1 });

# POST /users
post '/users' => sub {
    my $data = decode_json(request->body);  # Correctly decode JSON from the request body
    my $name = $data->{name};
    my $email = $data->{email};

    my $sth = $dbh->prepare("INSERT INTO users (name, email) VALUES (?, ?) RETURNING id");
    $sth->execute($name, $email);
    my ($id) = $sth->fetchrow_array();

    return encode_json({ id => $id, name => $name, email => $email }), { status => 201 };  # Encode response JSON
};

# GET /users
get '/users' => sub {
    my $sth = $dbh->prepare("SELECT id, name, email FROM users");
    $sth->execute();

    my @users;
    while (my $row = $sth->fetchrow_hashref()) {
        push @users, $row;
    }
    return encode_json(\@users), { status => 200 };  # Return the list of users
};

# GET /users/{id}
get '/users/:id' => sub {
    my $id = route_parameters->get('id');
    my $sth = $dbh->prepare("SELECT id, name, email FROM users WHERE id = ?");
    $sth->execute($id);

    my $user = $sth->fetchrow_hashref();
    return defined $user ? encode_json($user), { status => 200 } : halt(404);  # Return user or 404
};

# PUT /users/{id}
put '/users/:id' => sub {
    my $id = route_parameters->get('id');
    my $data = decode_json(request->body);  # Decode JSON

    my $sth = $dbh->prepare("UPDATE users SET name = ?, email = ? WHERE id = ?");
    my $result = $sth->execute($data->{name}, $data->{email}, $id);
    
    return $result ? (status(204), "") : halt(404);  # Return 204 or 404
};

# DELETE /users/{id}
del '/users/:id' => sub {
    my $id = route_parameters->get('id');
    my $sth = $dbh->prepare("DELETE FROM users WHERE id = ?");
    my $result = $sth->execute($id);

    return $result ? (status(204), "") : halt(404);  # Return 204 or 404
};

start;  # Start the Dancer application
