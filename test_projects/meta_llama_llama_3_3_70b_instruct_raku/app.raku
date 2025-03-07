
use v6.d;
use DBIish;
use Cro::HTTP::Router;
use Cro::HTTP::Server;
use JSON::Fast;

my %config = (
    host => 'host.docker.internal',
    port => 5432,
    database => 'complang',
    user => 'testuser',
    password => %*ENV<PGPASSWORD>
);

sub get-users($db) {
    my @users;
    for $db.query('SELECT * FROM users') -> $row {
        @users.push: {
            id => $row<id>,
            name => $row<name>,
            email => $row<email>
        };
    }
    return @users;
}

sub get-user($db, $id) {
    my $user;
    for $db.query("SELECT * FROM users WHERE id = $id") -> $row {
        $user = {
            id => $row<id>,
            name => $row<name>,
            email => $row<email>
        };
    }
    return $user;
}

sub create-user($db, $name, $email) {
    my $sth = $db.prepare('INSERT INTO users (name, email) VALUES (?,?)');
    $sth.execute($name, $email);
    my $id = $db.last-insert-id;
    return {
        id => $id,
        name => $name,
        email => $email
    };
}

sub update-user($db, $id, $name, $email) {
    my $sth = $db.prepare('UPDATE users SET name =?, email =? WHERE id =?');
    $sth.execute($name, $email, $id);
    return {
        id => $id,
        name => $name,
        email => $email
    };
}

sub delete-user($db, $id) {
    my $sth = $db.prepare('DELETE FROM users WHERE id =?');
    $sth.execute($id);
    return True;
}

my $db = DBIish.connect(%config);
my $router = Cro::HTTP::Router.new;

$router.add-route: POST => '/users', {
    request-body => 'application/json',
    content => -> $body, $req, $res {
        my %data = from-json($body);
        my $user = create-user($db, %data<name>, %data<email>);
        $res.status = 201;
        $res.send: to-json($user);
    }
};

$router.add-route: GET => '/users', {
    content => -> $req, $res {
        my @users = get-users($db);
        $res.send: to-json(@users);
    }
};

$router.add-route: GET => '/users/{id}', {
    content => -> $req, $res {
        my $id = $req.path-parts[1];
        my $user = get-user($db, $id);
        if $user {
            $res.send: to-json($user);
        } else {
            $res.status = 404;
            $res.send: 'User not found';
        }
    }
};

$router.add-route: PUT => '/users/{id}', {
    request-body => 'application/json',
    content => -> $body, $req, $res {
        my %data = from-json($body);
        my $id = $req.path-parts[1];
        my $user = update-user($db, $id, %data<name>, %data<email>);
        if $user {
            $res.send: to-json($user);
        } else {
            $res.status = 404;
            $res.send: 'User not found';
        }
    }
};

$router.add-route: DELETE => '/users/{id}', {
    content => -> $req, $res {
        my $id = $req.path-parts[1];
        my $success = delete-user($db, $id);
        if $success {
            $res.status = 200;
            $res.send: 'User deleted';
        } else {
            $res.status = 404;
            $res.send: 'User not found';
        }
    }
};

my $server = Cro::HTTP::Server.new($router);
$server.listen: 8080;
