
#!/usr/bin/env perl

use Mojolicious::Lite;
use DBI;
use Encode qw(decode encode);
use Data::Dumper;

# Read password from environment variable
my $pg_password = $ENV{PGPASSWORD} || die "PGPASSWORD environment variable not set";

# Database connection
my $dbh = DBI->connect("dbi:Pg:dbname=complang;host=host.docker.internal;port=5432", "testuser", $pg_password, { RaiseError => 1, AutoCommit => 1 });


# Helper function to fetch all users
sub get_all_users {
    my $sql = "SELECT id, name, email FROM users";
    my $sth = $dbh->prepare($sql);
    $sth->execute();
    my @users;
    while (my $row = $sth->fetchrow_hashref()) {
        push @users, $row;
    }
    $sth->finish();
    return \@users;
}

# Helper function to fetch a single user by ID
sub get_user {
    my ($id) = @_;
    my $sql = "SELECT id, name, email FROM users WHERE id = ?";
    my $sth = $dbh->prepare($sql);
    $sth->execute($id);
    my $user = $sth->fetchrow_hashref();
    $sth->finish();
    return $user;
}

# Helper function to create a user
sub create_user {
    my ($name, $email) = @_;
    my $sql = "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id";
    my $sth = $dbh->prepare($sql);
    $sth->execute($name, $email);
    my $id = $sth->fetchrow_arrayref()->[0];
    $sth->finish();
    return $id;
}

# Helper function to update a user
sub update_user {
    my ($id, $name, $email) = @_;
    my $sql = "UPDATE users SET name = ?, email = ? WHERE id = ?";
    my $sth = $dbh->prepare($sql);
    $sth->execute($name, $email, $id);
    my $rows_affected = $sth->rows;
    $sth->finish();
    return $rows_affected;
}

# Helper function to delete a user
sub delete_user {
    my ($id) = @_;
    my $sql = "DELETE FROM users WHERE id = ?";
    my $sth = $dbh->prepare($sql);
    $sth->execute($id);
    my $rows_affected = $sth->rows;
    $sth->finish();
    return $rows_affected;
}

# Route for creating a user (POST /users)
post '/users' => sub {
    my $self = shift;
    my $json = $self->req->json;
    my $name = $json->{name};
    my $email = $json->{email};

    if (not defined $name || not defined $email) {
        return $self->render_json({ error => "Missing name or email" }, { status => 400 });
    }

    my $id = create_user($name, $email);

    my $user = { id => $id, name => $name, email => $email };
    return $self->render_json($user, { status => 201 });
};

# Route for listing all users (GET /users)
get '/users' => sub {
    my $self = shift;
    my $users = get_all_users();
    return $self->render_json($users);
};

# Route for getting a single user (GET /users/:id)
get '/users/:id' => sub {
    my $self = shift;
    my $id = $self->param('id');
    my $user = get_user($id);

    if ($user) {
        return $self->render_json($user);
    } else {
        return $self->render_json({ error => "User not found" }, { status => 404 });
    }
};

# Route for updating a user (PUT /users/:id)
put '/users/:id' => sub {
    my $self = shift;
    my $id = $self->param('id');
    my $json = $self->req->json;
    my $name = $json->{name};
    my $email = $json->{email};

   if (not defined $name || not defined $email) {
        return $self->render_json({ error => "Missing name or email" }, { status => 400 });
    }

    my $rows_affected = update_user($id, $name, $email);

    if ($rows_affected > 0) {
      $self->res->code(204);
      return $self->render_json({});
    } else {
        return $self->render_json({ error => "User not found" }, { status => 404 });
    }
};

# Route for deleting a user (DELETE /users/:id)
del '/users/:id' => sub {
    my $self = shift;
    my $id = $self->param('id');
    my $rows_affected = delete_user($id);

    if ($rows_affected > 0) {
        $self->res->code(204);
        return $self->render_json({});

    } else {
        return $self->render_json({ error => "User not found" }, { status => 404 });
    }
};

app->start;
