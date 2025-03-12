#!/usr/bin/env perl

use strict;
use warnings;
use Mojolicious::Lite;
use DBI;

# Configuration
my $db_host = 'host.docker.internal';
my $db_port = '5432';
my $db_name = 'complang';
my $db_user = 'testuser';
my $db_pass = $ENV{PGPASSWORD} || die "PGPASSWORD environment variable not set";

# Connect to the database
my $dbh = DBI->connect(
    "dbi:Pg:dbname=$db_name;host=$db_host;port=$db_port",
    $db_user,
    $db_pass,
    { RaiseError => 1, AutoCommit => 1 }
) or die "Could not connect to database: $DBI::errstr";

# Helper function to get DB connection
helper db => sub {
    return $dbh;
};

# Create a new user
post '/users' => sub {
    my $c = shift;
    
    my $user_data = $c->req->json || {};
    
    unless ($user_data->{name} && $user_data->{email}) {
        return $c->render(json => { error => 'Name and email are required' }, status => 400);
    }
    
    my $sth = $c->db->prepare('INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email');
    $sth->execute($user_data->{name}, $user_data->{email});
    
    my $result = $sth->fetchrow_hashref();
    $sth->finish();
    
    $c->render(json => $result, status => 201);
};

# Get all users
get '/users' => sub {
    my $c = shift;
    
    my $sth = $c->db->prepare('SELECT id, name, email FROM users');
    $sth->execute();
    
    my @users;
    while (my $row = $sth->fetchrow_hashref()) {
        push @users, $row;
    }
    
    $c->render(json => \@users, status => 200);
};

# Get a single user
get '/users/:id' => sub {
    my $c = shift;
    my $id = $c->param('id');
    
    my $sth = $c->db->prepare('SELECT id, name, email FROM users WHERE id = ?');
    $sth->execute($id);
    
    my $user = $sth->fetchrow_hashref();
    $sth->finish();
    
    if ($user) {
        $c->render(json => $user, status => 200);
    } else {
        $c->render(json => { error => 'User not found' }, status => 404);
    }
};

# Update a user
put '/users/:id' => sub {
    my $c = shift;
    my $id = $c->param('id');
    my $user_data = $c->req->json || {};
    
    unless ($user_data->{name} && $user_data->{email}) {
        return $c->render(json => { error => 'Name and email are required' }, status => 400);
    }
    
    my $check_sth = $c->db->prepare('SELECT id FROM users WHERE id = ?');
    $check_sth->execute($id);
    my $exists = $check_sth->fetchrow_arrayref();
    $check_sth->finish();
    
    unless ($exists) {
        return $c->render(json => { error => 'User not found' }, status => 404);
    }
    
    my $sth = $c->db->prepare('UPDATE users SET name = ?, email = ? WHERE id = ?');
    $sth->execute($user_data->{name}, $user_data->{email}, $id);
    $sth->finish();
    
    $c->rendered(204); # No Content
};

# Delete a user
del '/users/:id' => sub {
    my $c = shift;
    my $id = $c->param('id');
    
    my $check_sth = $c->db->prepare('SELECT id FROM users WHERE id = ?');
    $check_sth->execute($id);
    my $exists = $check_sth->fetchrow_arrayref();
    $check_sth->finish();
    
    unless ($exists) {
        return $c->render(json => { error => 'User not found' }, status => 404);
    }
    
    my $sth = $c->db->prepare('DELETE FROM users WHERE id = ?');
    $sth->execute($id);
    $sth->finish();
    
    $c->rendered(204); # No Content
};

# Start the app
app->start('daemon', '-l', 'http://*:8080');
