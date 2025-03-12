
from fastapi import FastAPI, HTTPException, Response
from pydantic import BaseModel
import os
import psycopg2
from psycopg2.extras import RealDictCursor
import uvicorn

app = FastAPI()

# Define the User model
class User(BaseModel):
    name: str
    email: str

class UserResponse(BaseModel):
    id: int
    name: str
    email: str

# Database connection function
def get_db_connection():
    conn = psycopg2.connect(
        host="host.docker.internal",
        database="complang",
        user="testuser",
        password=os.environ.get("PGPASSWORD")
    )
    conn.autocommit = True
    return conn

# Create a user
@app.post("/users", status_code=201, response_model=UserResponse)
def create_user(user: User):
    conn = get_db_connection()
    cursor = conn.cursor(cursor_factory=RealDictCursor)
    cursor.execute(
        "INSERT INTO users (name, email) VALUES (%s, %s) RETURNING id, name, email",
        (user.name, user.email)
    )
    new_user = cursor.fetchone()
    cursor.close()
    conn.close()
    return new_user

# Get all users
@app.get("/users", response_model=list[UserResponse])
def get_users():
    conn = get_db_connection()
    cursor = conn.cursor(cursor_factory=RealDictCursor)
    cursor.execute("SELECT id, name, email FROM users")
    users = cursor.fetchall()
    cursor.close()
    conn.close()
    return users

# Get a user by ID
@app.get("/users/{user_id}", response_model=UserResponse)
def get_user(user_id: int):
    conn = get_db_connection()
    cursor = conn.cursor(cursor_factory=RealDictCursor)
    cursor.execute("SELECT id, name, email FROM users WHERE id = %s", (user_id,))
    user = cursor.fetchone()
    cursor.close()
    conn.close()
    
    if user is None:
        raise HTTPException(status_code=404, detail="User not found")
    
    return user

# Update a user
@app.put("/users/{user_id}", status_code=204)
def update_user(user_id: int, user: User):
    conn = get_db_connection()
    cursor = conn.cursor()
    cursor.execute(
        "UPDATE users SET name = %s, email = %s WHERE id = %s RETURNING id",
        (user.name, user.email, user_id)
    )
    updated = cursor.fetchone()
    cursor.close()
    conn.close()
    
    if updated is None:
        raise HTTPException(status_code=404, detail="User not found")
    
    return Response(status_code=204)

# Delete a user
@app.delete("/users/{user_id}", status_code=204)
def delete_user(user_id: int):
    conn = get_db_connection()
    cursor = conn.cursor()
    cursor.execute("DELETE FROM users WHERE id = %s RETURNING id", (user_id,))
    deleted = cursor.fetchone()
    cursor.close()
    conn.close()
    
    if deleted is None:
        raise HTTPException(status_code=404, detail="User not found")
    
    return Response(status_code=204)

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8080)
