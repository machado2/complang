
#!/bin/bash
set -e

psql -v ON_ERROR_STOP=1 <<-EOSQL
    CREATE TABLE IF NOT EXISTS users (
        id SERIAL PRIMARY KEY,
        name TEXT NOT NULL,
        email TEXT NOT NULL
    );
EOSQL
