#!/usr/bin/env raku
use Cro::HTTP::Router;
use Cro::HTTP::Server;
use JSON::Fast;
use DBIish;

# Build DSN string including host and port
my $dsn = "dbname=complang;host=host.docker.internal;port=5432";
my $dbh = DBIish.connect('Pg', $dsn, :user('testuser'), :password($*ENV<PGPASSWORD>));

my $app = route {
    # POST /users: Create a new user.
    post '/users' => sub ($req) {
        my %data = from-json($req.content);
        my $name  = %data<name>;
        my $email = %data<email>;
        my $sth = $dbh.prepare("INSERT INTO users (name, email) VALUES (?, ?) RETURNING id");
        $sth.execute($name, $email);
        my @row = $sth.fetchrow_array;
        my $new_id = @row[0];
        $sth.finish;
        return { status => 201, content => to-json({ id => $new_id, name => $name, email => $email }) };
    };

    # GET /users: Retrieve all users.
    get '/users' => sub ($req) {
        my $sth = $dbh.prepare("SELECT id, name, email FROM users");
        $sth.execute();
        my @users;
        while my $row = $sth.fetchrow_hashref {
            @users.push($row);
        }
        $sth.finish;
        return { status => 200, content => to-json(@users) };
    };

    # GET /users/{id}: Retrieve a single user by id.
    get '/users/:id' => sub ($req, $capture) {
        my $user_id = $capture<id>;
        my $sth = $dbh.prepare("SELECT id, name, email FROM users WHERE id = ?");
        $sth.execute($user_id);
        my $user = $sth.fetchrow_hashref;
        $sth.finish;
        if $user {
            return { status => 200, content => to-json($user) };
        } else {
            return { status => 404, content => "Not Found" };
        }
    };

    # PUT /users/{id}: Update a user.
    put '/users/:id' => sub ($req, $capture) {
        my $user_id = $capture<id>;
        my %data = from-json($req.content);
        my $name  = %data<name>;
        my $email = %data<email>;
        my $sth = $dbh.prepare("UPDATE users SET name = ?, email = ? WHERE id = ?");
        $sth.execute($name, $email, $user_id);
        my $rows = $sth.rows;
        $sth.finish;
        if $rows == 0 {
            return { status => 404, content => "Not Found" };
        }
        return { status => 200, content => "" };
    };

    # DELETE /users/{id}: Delete a user.
    delete '/users/:id' => sub ($req, $capture) {
        my $user_id = $capture<id>;
        my $sth = $dbh.prepare("DELETE FROM users WHERE id = ?");
        $sth.execute($user_id);
        my $rows = $sth.rows;
        $sth.finish;
        if $rows == 0 {
            return { status => 404, content => "Not Found" };
        }
        return { status => 200, content => "" };
    };
};

# Start the Cro server listening on 0.0.0.0 at port 8080
my $server = Cro::HTTP::Server.new(host => "0.0.0.0", port => 8080, applications => $app);
$server.start;
say "Server is running on port 8080";
react whenever Promise.infinite {} ;
