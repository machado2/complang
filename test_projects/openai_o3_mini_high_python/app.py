from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from sqlalchemy import create_engine, Column, Integer, String
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
from typing import List
import os

# Get the PostgreSQL password from environment variable
pg_password = os.environ.get("PGPASSWORD")
if not pg_password:
    raise Exception("PGPASSWORD environment variable not set")

DATABASE_URL = f"postgresql://testuser:{pg_password}@host.docker.internal:5432/complang"

# SQLAlchemy engine and session
engine = create_engine(DATABASE_URL)
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)
Base = declarative_base()

# User model mapping to the 'users' table
class User(Base):
    __tablename__ = "users"
    id = Column(Integer, primary_key=True, index=True)
    name = Column(String, nullable=False)
    email = Column(String, nullable=False)

# Ensure the table exists
Base.metadata.create_all(bind=engine)

app = FastAPI()

# Pydantic models for input and output
class UserCreate(BaseModel):
    name: str
    email: str

class UserResponse(BaseModel):
    id: int
    name: str
    email: str

    class Config:
        orm_mode = True

# POST /users: Create a new user
@app.post("/users", response_model=UserResponse, status_code=201)
def create_user(user: UserCreate):
    db = SessionLocal()
    db_user = User(name=user.name, email=user.email)
    db.add(db_user)
    db.commit()
    db.refresh(db_user)
    db.close()
    return db_user

# GET /users: Retrieve all users
@app.get("/users", response_model=List[UserResponse])
def get_users():
    db = SessionLocal()
    users = db.query(User).all()
    db.close()
    return users

# GET /users/{user_id}: Retrieve a user by id
@app.get("/users/{user_id}", response_model=UserResponse)
def get_user(user_id: int):
    db = SessionLocal()
    user = db.query(User).filter(User.id == user_id).first()
    db.close()
    if user is None:
        raise HTTPException(status_code=404, detail="User not found")
    return user

# PUT /users/{user_id}: Update an existing user
@app.put("/users/{user_id}", response_model=UserResponse)
def update_user(user_id: int, user_data: UserCreate):
    db = SessionLocal()
    user = db.query(User).filter(User.id == user_id).first()
    if user is None:
        db.close()
        raise HTTPException(status_code=404, detail="User not found")
    user.name = user_data.name
    user.email = user_data.email
    db.commit()
    db.refresh(user)
    db.close()
    return user

# DELETE /users/{user_id}: Delete a user
@app.delete("/users/{user_id}", status_code=204)
def delete_user(user_id: int):
    db = SessionLocal()
    user = db.query(User).filter(User.id == user_id).first()
    if user is None:
        db.close()
        raise HTTPException(status_code=404, detail="User not found")
    db.delete(user)
    db.commit()
    db.close()
    return

if __name__ == "__main__":
    import uvicorn
    uvicorn.run("app:app", host="0.0.0.0", port=8080)
