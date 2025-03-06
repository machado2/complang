from typing import List, Optional

from fastapi import FastAPI, HTTPException, status
from fastapi.params import Depends
from pydantic import BaseModel, Field
import os
import psycopg2
from psycopg2 import OperationalError

app = FastAPI()

# Database connection settings
DATABASE_URL = "postgresql://testuser:{password}@host.docker.internal:5432/complang".format(password=os.environ.get("PGPASSWORD", "Saloon5-Moody-Observing"))


# Pydantic models
class UserBase(BaseModel):
    name: str = Field(..., min_length=1, max_length=100)
    email: str = Field(..., min_length=5, max_length=100)


class UserCreate(UserBase):
    pass


class User(UserBase):
    id: int

    class Config:
        orm_mode = True


# Dependency to get database connection
def get_db():
    try:
        conn = psycopg2.connect(DATABASE_URL)
        db = conn.cursor()
        yield db, conn
    except OperationalError as e:
        print(f"Database connection error: {e}")
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail="Database connection failed")
    finally:
        if hasattr(db, 'close'):
            db.close()
        if hasattr(conn, 'close'):
            conn.close()


# API endpoints
@app.post("/users", response_model=User, status_code=status.HTTP_201_CREATED)
async def create_user(user: UserCreate, db_conn: tuple = Depends(get_db)):
    db, conn = db_conn
    query = """INSERT INTO users (name, email) VALUES (%s, %s) RETURNING id, name, email;"""
    try:
        db.execute(query, (user.name, user.email))
        new_user = db.fetchone()
        conn.commit()
        return User(id=new_user[0], name=new_user[1], email=new_user[2])
    except Exception as e:
        conn.rollback()
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(e))


@app.get("/users", response_model=List[User])
async def read_users(db_conn: tuple = Depends(get_db)):
    db, conn = db_conn
    query = "SELECT id, name, email FROM users;"

    try:
        db.execute(query)
        users = db.fetchall()
        return [User(id=user[0], name=user[1], email=user[2]) for user in users]
    except Exception as e:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(e))


@app.get("/users/{user_id}", response_model=User)
async def read_user(user_id: int, db_conn: tuple = Depends(get_db)):
    db, conn = db_conn
    query = "SELECT id, name, email FROM users WHERE id = %s;"
    try:
        db.execute(query, (user_id,))
        user = db.fetchone()
        if user:
            return User(id=user[0], name=user[1], email=user[2])
        else:
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="User not found")
    except Exception as e:
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(e))


@app.put("/users/{user_id}", status_code=status.HTTP_204_NO_CONTENT)
async def update_user(user_id: int, user: UserBase, db_conn: tuple = Depends(get_db)):
    db, conn = db_conn
    query = """UPDATE users SET name = %s, email = %s WHERE id = %s;"""
    try:
        db.execute(query, (user.name, user.email, user_id))
        if db.rowcount == 0:
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="User not found")
        conn.commit()
        return
    except Exception as e:
        conn.rollback()
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(e))


@app.delete("/users/{user_id}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_user(user_id: int, db_conn: tuple = Depends(get_db)):
    db, conn = db_conn
    query = "DELETE FROM users WHERE id = %s;"
    try:
        db.execute(query, (user_id,))
        if db.rowcount == 0:
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="User not found")
        conn.commit()
        return
    except Exception as e:
        conn.rollback()
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=str(e))
