import os

def get_pg_password():
    return os.getenv("PGPASSWORD")