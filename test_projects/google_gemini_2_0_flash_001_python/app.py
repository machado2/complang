
import os
import uuid
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import psycopg2
import time

app = FastAPI()

DATABASE_HOST = os.environ.get("DATABASE_HOST", "host.docker.internal")
DATABASE_PORT = os.environ.get("DATABASE_PORT", "5432")
DB_NAME = os.environ.get("DB_NAME", "test_google_gemini_2_0_flash_001_python")
DB_USER = os.environ.get("DB_USER", "postgres")
DB_PASSWORD = os.environ.get("PGPASSWORD", "test")

def connect_to_db():
    max_retries = 5
    retry_delay = 2
    for attempt in range(max_retries):
        try:
            conn = psycopg2.connect(
                host=DATABASE_HOST,
                port=DATABASE_PORT,
                database=DB_NAME,
                user=DB_USER,
                password=DB_PASSWORD
            )
            return conn
        except Exception as e:
            print(f"Attempt {attempt + 1}: Error connecting to database: {e}")
            if attempt < max_retries - 1:
                time.sleep(retry_delay)
            else:
                raise

def check_table_exists(conn, table_name):
    cur = conn.cursor()
    cur.execute(f"SELECT EXISTS (SELECT 1 FROM pg_tables WHERE tablename = '{table_name}')")
    return cur.fetchone()[0]

def create_table():
    conn = connect_to_db()
    cur = conn.cursor()
    try:
        cur.execute("""
            CREATE TABLE users (
                id SERIAL PRIMARY KEY,
                name TEXT,
                email TEXT
            )
        """)
        conn.commit()
    except Exception as e:
        print(f"Error creating table: {e}")
        conn.rollback()
    finally:
        cur.close()
        conn.close()

conn = connect_to_db()
users_table_exists = check_table_exists(conn, 'users')
conn.close()

if not users_table_exists:
    create_table()

class User(BaseModel):
    id: int = None
    name: str
    email: str

class UserCreate(BaseModel):
    name: str
    email: str

@app.post("/users", status_code=201)
def create_user(user: UserCreate):
    conn = connect_to_db()
    cur = conn.cursor()
    try:
        cur.execute(
            "INSERT INTO users (name, email) VALUES (%s, %s) RETURNING id, name, email",
            (user.name, user.email)
        )
        new_user = cur.fetchone()
        conn.commit()
        return {"id": new_user[0], "name": new_user[1], "email": new_user[2]}
    except Exception as e:
        print(f"Error creating user: {e}")
        conn.rollback()
        raise HTTPException(status_code=500, detail="Failed to create user")
    finally:
        cur.close()
        conn.close()


@app.get("/users")
def list_users():
    conn = connect_to_db()
    cur = conn.cursor()
    try:
        cur.execute("SELECT id, name, email FROM users")
        users = cur.fetchall()
        user_list = []
        for user in users:
            user_list.append({"id": user[0], "name": user[1], "email": user[2]})
        return user_list
    except Exception as e:
        print(f"Error listing users: {e}")
        raise HTTPException(status_code=500, detail="Failed to list users")
    finally:
        cur.close()
        conn.close()


@app.get("/users/{user_id}")
def get_user(user_id: int):
    conn = connect_to_db()
    cur = conn.cursor()
    try:
        cur.execute("SELECT id, name, email FROM users WHERE id = %s", (user_id,))
        user = cur.fetchone()
        if user:
            return {"id": user[0], "name": user[1], "email": user[2]}
        else:
            raise HTTPException(status_code=404, detail="User not found")
    except Exception as e:
        print(f"Error getting user: {e}")
        raise HTTPException(status_code=500, detail="Failed to get user")
    finally:
        cur.close()
        conn.close()


@app.put("/users/{user_id}")
def update_user(user_id: int, user: UserCreate):
    conn = connect_to_db()
    cur = conn.cursor()
    try:
        cur.execute(
            "UPDATE users SET name = %s, email = %s WHERE id = %s",
            (user.name, user.email, user_id)
        )
        conn.commit()
        if cur.rowcount == 0:
            raise HTTPException(status_code=404, detail="User not found")
        return {} # 204 no content
    except Exception as e:
        print(f"Error updating user: {e}")
        conn.rollback()
        raise HTTPException(status_code=500, detail="Failed to update user")
    finally:
        cur.close()
        conn.close()


@app.delete("/users/{user_id}")
def delete_user(user_id: int):
    conn = connect_to_db()
    cur = conn.cursor()
    try:
        cur.execute("DELETE FROM users WHERE id = %s", (user_id,))
        conn.commit()
        if cur.rowcount == 0:
            raise HTTPException(status_code=404, detail="User not found")
        return {} # 204 no content
    except Exception as e:
        print(f"Error deleting user: {e}")
        conn.rollback()
        raise HTTPException(status_code=500, detail="Failed to delete user")
    finally:
        cur.close()
        conn.close()
