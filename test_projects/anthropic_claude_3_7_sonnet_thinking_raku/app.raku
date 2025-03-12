
use v6;
use Cro::HTTP::Router;
use Cro::HTTP::Server;
use DB::Pg;
use JSON::Fast;

# Configure database connection
my $host = 'host.docker.internal';
my $port = 5432;
my $database = 'complang';
my $user = 'testuser';
my $password = %*ENV<PGPASSWORD>;

my $pg = DB::Pg.new(
    conninfo => "host=$host port=$port dbname=$database user=$user password=$password"
);

# Create the routes
my $application = route {
    # Health check endpoint
    get -> {
        content 'application/json', { status => 'ok' };
    }

    # GET /users - List all users
    get -> 'users' {
        my @users = $pg.query('SELECT id, name, email FROM users').arrays.map(
            -> $row {
                {
                    id => $row[0],
                    name => $row[1],
                    email => $row[2]
                }
            }
        );
        content 'application/json', @users;
    }

    # GET /users/{id} - Get a single user
    get -> 'users', Int $id {
        my $user = $pg.query('SELECT id, name, email FROM users WHERE id = $1', $id).hash;
        if $user {
            content 'application/json', {
                id => $user<id>,
                name => $user<name>,
                email => $user<email>
            };
        } else {
            not-found;
        }
    }

    # POST /users - Create a new user
    post -> 'users' {
        request-body -> %json {
            my $name = %json<name>;
            my $email = %json<email>;
            
            unless $name && $email {
                bad-request('Name and email are required');
                return;
            }
            
            my $user = $pg.query('INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email',
                $name, $email).hash;
                
            created 'application/json', {
                id => $user<id>,
                name => $user<name>,
                email => $user<email>
            };
        }
    }

    # PUT /users/{id} - Update a user
    put -> 'users', Int $id {
        request-body -> %json {
            my $name = %json<name>;
            my $email = %json<email>;
            
            unless $name && $email {
                bad-request('Name and email are required');
                return;
            }
            
            my $count = $pg.query('UPDATE users SET name = $1, email = $2 WHERE id = $3',
                $name, $email, $id).rows;
                
            if $count {
                no-content;
            } else {
                not-found;
            }
        }
    }

    # DELETE /users/{id} - Delete a user
    delete -> 'users', Int $id {
        my $count = $pg.query('DELETE FROM users WHERE id = $1', $id).rows;
        
        if $count {
            no-content;
        } else {
            not-found;
        }
    }
}

# Start the server
my Cro::Service $service = Cro::HTTP::Server.new(
    :host('0.0.0.0'),
    :port(8080),
    :$application
);

$service.start;

say "API server started at http://0.0.0.0:8080";

# Keep the application running
react whenever signal(SIGINT) {
    say "Shutting down...";
    $service.stop;
    exit;
}
