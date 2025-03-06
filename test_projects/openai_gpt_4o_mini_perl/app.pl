#!/usr/bin/perl
use strict;
use warnings;
use JSON;
use DBI;
use Dancer2;

my $dbh = DBI->connect(
    'dbi:Pg:dbname=complang;host=host.docker.internal;port=5432',
    'testuser',
    $ENV{'PGPASSWORD'},
    { RaiseError => 1 }
);

post '/users' => sub {
    my $data = from_json(request->body);
    my $sth = $dbh->prepare('INSERT INTO users (name, email) VALUES (?, ?) RETURNING id');
    $sth->execute($data->{name}, $data->{email});
    my $id = $sth->fetchrow_array();
    return to_json({ id => $id, name => $data->{name}, email => $data->{email} }), 201;
};

get '/users' => sub {
    my $sth = $dbh->prepare('SELECT * FROM users');
    $sth->execute();
    my @users;
    while (my $row = $sth->fetchrow_hashref()) {
        push @users, $row;
    }
    return to_json(\@users);
};

get '/users/:id' => sub {
    my $id = params->{id};
    my $sth = $dbh->prepare('SELECT * FROM users WHERE id = ?');
    $sth->execute($id);
    my $user = $sth->fetchrow_hashref();
    return $user ? to_json($user) : status(404);
};

put '/users/:id' => sub {
    my $id = params->{id};
    my $data = from_json(request->body);
    my $sth = $dbh->prepare('UPDATE users SET name = ?, email = ? WHERE id = ?');
    my $rows_affected = $sth->execute($data->{name}, $data->{email}, $id);
    return $rows_affected ? status(204) : status(404);
};

delete '/users/:id' => sub {
    my $id = params->{id};
    my $sth = $dbh->prepare('DELETE FROM users WHERE id = ?');
    my $rows_affected = $sth->execute($id);
    return $rows_affected > 0 ? status(204) : status(404);
};

start;