#!/usr/bin/env raku
use v6;
use Cro::HTTP::Router;
use Cro::HTTP::Server;
use JSON::Fast;
use DBIish;

# Retrieve the database password from the environment
my $db_password = %?ENV<PGPASSWORD> // die "Environment variable PGPASSWORD not set";

# Connect to PostgreSQL (database "complang", user "testuser")
my $dbh = DBIish.connect("Pg", "complang", {
    host     => "host.docker.internal",
    port     => 5432,
    user     => "testuser",
    password => $db_password
});

my $api = route {
    # POST /users: Create a user.
    post "/users" => sub ($req) {
        my $body  = from-json($req.content);
        my $name  = $body<name>;
        my $email = $body<email>;
        my $sth   = $dbh.prepare("INSERT INTO users (name, email) VALUES (?, ?) RETURNING id");
        $sth.execute($name, $email);
        my $row = $sth.fetchrow_hash();
        my $id  = $row<id>;
        my %user = ( id => +$id, name => $name, email => $email );
        return response(201, :content(to-json(%user)), "Content-Type" => "application/json");
    },

    # GET /users: Return all users.
    get "/users" => sub ($req) {
        my @rows = $dbh.selectall_hash("SELECT id, name, email FROM users");
        return response(200, :content(to-json(@rows)), "Content-Type" => "application/json");
    },

    # GET /users/{id}: Return a specific user.
    get rx/^\/users\/(\d+)$/ => sub ($req, @captures) {
        my $id = @captures[0];
        my %user = $dbh.selectrow_hash("SELECT id, name, email FROM users WHERE id = ?", $id);
        if %user {
            return response(200, :content(to-json(%user)), "Content-Type" => "application/json");
        }
        else {
            return response(404, :content('{"error": "User not found"}'), "Content-Type" => "application/json");
        }
    },

    # PUT /users/{id}: Update an existing user.
    put rx/^\/users\/(\d+)$/ => sub ($req, @captures) {
        my $id   = @captures[0];
        my $body = from-json($req.content);
        my $name = $body<name>;
        my $email= $body<email>;
        my $rows = $dbh.do("UPDATE users SET name = ?, email = ? WHERE id = ?", $name, $email, $id);
        if $rows && $rows > 0 {
            my %user = $dbh.selectrow_hash("SELECT id, name, email FROM users WHERE id = ?", $id);
            return response(200, :content(to-json(%user)), "Content-Type" => "application/json");
        }
        else {
            return response(404, :content('{"error": "User not found"}'), "Content-Type" => "application/json");
        }
    },

    # DELETE /users/{id}: Delete a user.
    delete rx/^\/users\/(\d+)$/ => sub ($req, @captures) {
        my $id = @captures[0];
        my $rows = $dbh.do("DELETE FROM users WHERE id = ?", $id);
        if $rows && $rows > 0 {
            return response(200, :content('{"message": "User deleted"}'), "Content-Type" => "application/json");
        }
        else {
            return response(404, :content('{"error": "User not found"}'), "Content-Type" => "application/json");
        }
    }
};

# Start the Cro HTTP server on port 8080
Cro::HTTP::Server.listen($api, :port(8080));
