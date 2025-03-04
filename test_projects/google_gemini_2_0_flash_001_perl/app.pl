
#!/usr/bin/env perl

use strict;
use warnings;
use Mojolicious::Lite -signatures;
use DBI;
use JSON;
use Data::Dumper;
use Encode qw(decode encode);
use Encode::Detect;

# Load environment variables
my $password = $ENV{PGPASSWORD};

my $host     = 'host.docker.internal';
my $port     = 5432;
my $database = 'test_google_gemini_2_0_flash_001_perl';
my $user     = 'postgres';

# Database connection string
my $dsn = "dbi:Pg:dbname=$database;host=$host;port=$port";

# Function to connect to the database
sub db_connect {
    my $dbh = DBI->connect($dsn, $user, $password, { RaiseError => 1, AutoCommit => 0 });
    return $dbh;
}

# Function to create the users table if it doesn't exist
sub create_users_table {
    my ($dbh) = @_;
    my $sth = $dbh->prepare(qq{
        CREATE TABLE IF NOT EXISTS users (
            id SERIAL PRIMARY KEY,
            name TEXT,
            email TEXT
        )
    });
    $sth->execute();
}

# Check if table exists
sub table_exists {
    my ($dbh, $table_name) = @_;
    my $sth = $dbh->prepare(qq{
        SELECT EXISTS (
            SELECT 1
            FROM   pg_catalog.pg_tables
            WHERE  schemaname = 'public'
            AND    tablename = ?
        );
    });
    $sth->execute($table_name);
    my ($exists) = $sth->fetchrow_array();
    return $exists;
}

# Route for creating a user (POST /users)
post '/users' => sub ($c) {
    my $dbh = db_connect();
    eval {
        unless (table_exists($dbh, 'users')) {
            create_users_table($dbh);
        }
        my $json = decode_json $c->req->body;
        my $name  = $json->{name};
        my $email = $json->{email};

        my $sth = $dbh->prepare(qq{
            INSERT INTO users (name, email) VALUES (?, ?) RETURNING id
        });
        $sth->execute($name, $email);
        my ($id) = $sth->fetchrow_array();
        $dbh->commit();
        $sth->finish();
        $dbh->disconnect();
        $c->res->code(201);
        $c->render(json => { id => $id, name => $name, email => $email });
    };
    if ($@) {
        $dbh->rollback();
        $dbh->disconnect();
        $c->res->code(500);
        return $c->render(text => "DB Error: $@");
    }
};

# Route for getting all users (GET /users)
get '/users' => sub ($c) {
    my $dbh = db_connect();
    eval {
        unless (table_exists($dbh, 'users')) {
            create_users_table($dbh);
        }
        my $sth = $dbh->prepare(qq{
            SELECT id, name, email FROM users
        });
        $sth->execute();

        my @users;
        while (my $row = $sth->fetchrow_hashref()) {
            push @users, { id => $row->{id}, name => $row->{name}, email => $row->{email} };
        }
        $dbh->commit();
        $sth->finish();
        $dbh->disconnect();

        $c->render(json => \@users);
    };
    if ($@) {
        $dbh->rollback();
        $dbh->disconnect();
        $c->res->code(500);
        return $c->render(text => "DB Error: $@");
    }
};

# Route for getting a specific user (GET /users/:id)
get '/users/:id' => sub ($c) {
    my $dbh = db_connect();
    eval {
        unless (table_exists($dbh, 'users')) {
            create_users_table($dbh);
        }
        my $id = $c->param('id');
        my $sth = $dbh->prepare(qq{
            SELECT id, name, email FROM users WHERE id = ?
        });
        $sth->execute($id);

        my $user = $sth->fetchrow_hashref();
        $dbh->commit();
        $sth->finish();
        $dbh->disconnect();

        if ($user) {
            $c->render(json => { id => $user->{id}, name => $user->{name}, email => $user->{email} });
        } else {
            $c->res->code(404)->render(text => 'User not found');
        }
    };
    if ($@) {
        $dbh->rollback();
        $dbh->disconnect();
        $c->res->code(500);
        return $c->render(text => "DB Error: $@");
    }
};

# Route for updating a user (PUT /users/:id)
put '/users/:id' => sub ($c) {
   my $dbh = db_connect();
    eval {
        unless (table_exists($dbh, 'users')) {
            create_users_table($dbh);
        }
        my $id = $c->param('id');
        my $json = decode_json $c->req->body;
        my $name  = $json->{name};
        my $email = $json->{email};

        my $sth = $dbh->prepare(qq{
            UPDATE users SET name = ?, email = ? WHERE id = ?
        });
        $sth->execute($name, $email, $id);

        my $rows_affected = $sth->rows;
        $dbh->commit();
        $sth->finish();
        $dbh->disconnect();

        if ($rows_affected > 0) {
            $c->res->code(200);
            $c->res->headers->header('Content-Length' => 0);
            $c->render(text => '');

        } else {
            $c->res->code(404)->render(text => 'User not found');
        }
    };
    if ($@) {
        $dbh->rollback();
        $dbh->disconnect();
        $c->res->code(500);
        return $c->render(text => "DB Error: $@");
    }
};

# Route for deleting a user (DELETE /users/:id)
del '/users/:id' => sub ($c) {
    my $dbh = db_connect();
    eval {
        unless (table_exists($dbh, 'users')) {
            create_users_table($dbh);
        }
        my $id = $c->param('id');
        my $sth = $dbh->prepare(qq{
            DELETE FROM users WHERE id = ?
        });
        $sth->execute($id);
        my $rows_affected = $sth->rows;
        $dbh->commit();
        $sth->finish();
        $dbh->disconnect();

        if ($rows_affected > 0) {
           $c->res->code(200);
            $c->res->headers->header('Content-Length' => 0);
            $c->render(text => '');
        } else {
            $c->res->code(404)->render(text => 'User not found');
        }
   };
    if ($@) {
        $dbh->rollback();
        $dbh->disconnect();
        $c->res->code(500);
        return $c->render(text => "DB Error: $@");
    }
};

app->start;
