
use strict;
use warnings;
use Plack::Builder;
use Plack::Request;
use JSON;
use DBI;

my $app = sub {
    my $env = shift;
    my $req = Plack::Request->new($env);
    my $res = $req->new_response(200);

    if ($req->path eq '/users') {
        if ($req->method eq 'POST') {
            my $data = JSON::decode_json($req->content);
            my $dbh = DBI->connect(
                "dbi:Pg:dbname=complang;host=host.docker.internal;port=5432",
                "testuser",
                $ENV{PGPASSWORD},
                { RaiseError => 1 },
            );
            my $sth = $dbh->prepare('INSERT INTO users (name, email) VALUES (?,?)');
            $sth->execute($data->{name}, $data->{email});
            my $id = $sth->last_insert_id(undef, undef, undef, undef);
            $res->code(201);
            $res->content(JSON::encode_json({ id => $id, name => $data->{name}, email => $data->{email} }));
        } elsif ($req->method eq 'GET') {
            my $dbh = DBI->connect(
                "dbi:Pg:dbname=complang;host=host.docker.internal;port=5432",
                "testuser",
                $ENV{PGPASSWORD},
                { RaiseError => 1 },
            );
            my $sth = $dbh->prepare('SELECT * FROM users');
            $sth->execute();
            my @users;
            while (my $row = $sth->fetchrow_hashref) {
                push @users, { id => $row->{id}, name => $row->{name}, email => $row->{email} };
            }
            $res->content(JSON::encode_json(\@users));
        }
    } elsif ($req->path =~ m{/users/(\d+)}) {
        my $id = $1;
        if ($req->method eq 'GET') {
            my $dbh = DBI->connect(
                "dbi:Pg:dbname=complang;host=host.docker.internal;port=5432",
                "testuser",
                $ENV{PGPASSWORD},
                { RaiseError => 1 },
            );
            my $sth = $dbh->prepare('SELECT * FROM users WHERE id =?');
            $sth->execute($id);
            my $row = $sth->fetchrow_hashref;
            if ($row) {
                $res->code(200);
                $res->content(JSON::encode_json({ id => $row->{id}, name => $row->{name}, email => $row->{email} }));
            } else {
                $res->code(404);
            }
        } elsif ($req->method eq 'PUT') {
            my $data = JSON::decode_json($req->content);
            my $dbh = DBI->connect(
                "dbi:Pg:dbname=complang;host=host.docker.internal;port=5432",
                "testuser",
                $ENV{PGPASSWORD},
                { RaiseError => 1 },
            );
            my $sth = $dbh->prepare('UPDATE users SET name =?, email =? WHERE id =?');
            $sth->execute($data->{name}, $data->{email}, $id);
            $res->code(200);
        } elsif ($req->method eq 'DELETE') {
            my $dbh = DBI->connect(
                "dbi:Pg:dbname=complang;host=host.docker.internal;port=5432",
                "testuser",
                $ENV{PGPASSWORD},
                { RaiseError => 1 },
            );
            my $sth = $dbh->prepare('DELETE FROM users WHERE id =?');
            $sth->execute($id);
            $res->code(200);
        }
    }

    return $res->finalize;
};

builder {
    enable "Plack::Middleware::ContentLength";
    $app;
};

