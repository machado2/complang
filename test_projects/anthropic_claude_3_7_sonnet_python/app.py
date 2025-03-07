
import os
import json
import psycopg2
from psycopg2.extras import RealDictCursor
from flask import Flask, request, jsonify

app = Flask(__name__)

def get_db_connection():
    # Get password from environment variable
    password = os.environ.get('PGPASSWORD')
    if not password:
        app.logger.error("PGPASSWORD environment variable not set")
        raise ValueError("Database password not provided. Set PGPASSWORD environment variable.")
    
    try:
        # Connect to the PostgreSQL database
        conn = psycopg2.connect(
            host='host.docker.internal',
            database='complang',
            user='testuser',
            password=password
        )
        
        # Return dictionary-like results
        conn.cursor_factory = RealDictCursor
        
        return conn
    except psycopg2.Error as e:
        app.logger.error(f"Database connection error: {e}")
        raise

@app.route('/users', methods=['POST'])
def create_user():
    data = request.get_json()
    
    # Validate required fields
    if not data or 'name' not in data or 'email' not in data:
        return jsonify({"error": "Name and email are required"}), 400
    
    try:
        conn = get_db_connection()
        cursor = conn.cursor()
        
        try:
            # Insert user into the database
            cursor.execute(
                "INSERT INTO users (name, email) VALUES (%s, %s) RETURNING id, name, email;",
                (data['name'], data['email'])
            )
            
            # Get the created user
            user = cursor.fetchone()
            conn.commit()
            
            return jsonify(dict(user)), 201
        
        except Exception as e:
            conn.rollback()
            app.logger.error(f"Error creating user: {e}")
            return jsonify({"error": str(e)}), 500
        
        finally:
            cursor.close()
            conn.close()
    except Exception as e:
        app.logger.error(f"Database connection error: {e}")
        return jsonify({"error": "Database connection error"}), 500

@app.route('/users', methods=['GET'])
def get_users():
    try:
        conn = get_db_connection()
        cursor = conn.cursor()
        
        try:
            cursor.execute("SELECT id, name, email FROM users;")
            users = cursor.fetchall()
            
            return jsonify([dict(user) for user in users]), 200
        
        except Exception as e:
            app.logger.error(f"Error retrieving users: {e}")
            return jsonify({"error": str(e)}), 500
        
        finally:
            cursor.close()
            conn.close()
    except Exception as e:
        app.logger.error(f"Database connection error: {e}")
        return jsonify({"error": "Database connection error"}), 500

@app.route('/users/<int:id>', methods=['GET'])
def get_user(id):
    try:
        conn = get_db_connection()
        cursor = conn.cursor()
        
        try:
            cursor.execute("SELECT id, name, email FROM users WHERE id = %s;", (id,))
            user = cursor.fetchone()
            
            if user is None:
                return jsonify({"error": "User not found"}), 404
            
            return jsonify(dict(user)), 200
        
        except Exception as e:
            app.logger.error(f"Error retrieving user {id}: {e}")
            return jsonify({"error": str(e)}), 500
        
        finally:
            cursor.close()
            conn.close()
    except Exception as e:
        app.logger.error(f"Database connection error: {e}")
        return jsonify({"error": "Database connection error"}), 500

@app.route('/users/<int:id>', methods=['PUT'])
def update_user(id):
    data = request.get_json()
    
    # Validate required fields
    if not data or 'name' not in data or 'email' not in data:
        return jsonify({"error": "Name and email are required"}), 400
    
    try:
        conn = get_db_connection()
        cursor = conn.cursor()
        
        try:
            # Check if user exists
            cursor.execute("SELECT id FROM users WHERE id = %s;", (id,))
            if cursor.fetchone() is None:
                return jsonify({"error": "User not found"}), 404
            
            # Update user
            cursor.execute(
                "UPDATE users SET name = %s, email = %s WHERE id = %s;",
                (data['name'], data['email'], id)
            )
            
            conn.commit()
            
            return "", 204
        
        except Exception as e:
            conn.rollback()
            app.logger.error(f"Error updating user {id}: {e}")
            return jsonify({"error": str(e)}), 500
        
        finally:
            cursor.close()
            conn.close()
    except Exception as e:
        app.logger.error(f"Database connection error: {e}")
        return jsonify({"error": "Database connection error"}), 500

@app.route('/users/<int:id>', methods=['DELETE'])
def delete_user(id):
    try:
        conn = get_db_connection()
        cursor = conn.cursor()
        
        try:
            # Check if user exists
            cursor.execute("SELECT id FROM users WHERE id = %s;", (id,))
            if cursor.fetchone() is None:
                return jsonify({"error": "User not found"}), 404
            
            # Delete user
            cursor.execute("DELETE FROM users WHERE id = %s;", (id,))
            
            conn.commit()
            
            return "", 204
        
        except Exception as e:
            conn.rollback()
            app.logger.error(f"Error deleting user {id}: {e}")
            return jsonify({"error": str(e)}), 500
        
        finally:
            cursor.close()
            conn.close()
    except Exception as e:
        app.logger.error(f"Database connection error: {e}")
        return jsonify({"error": "Database connection error"}), 500

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8080)
