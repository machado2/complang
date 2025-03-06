
import os
import psycopg2
from flask import Flask, request, jsonify
from flask_restful import Resource, Api

app = Flask(__name__)
api = Api(app)

DATABASE_URL = 'host.docker.internal:5432'
DATABASE_NAME = 'test_google_gemini_2_0_flash_001_python'
DATABASE_USER = 'postgres'
DATABASE_PASSWORD = os.environ.get('PGPASSWORD')

def connect_to_db():
    conn = None
    try:
        conn = psycopg2.connect(
            host=DATABASE_URL.split(':')[0],
            port=DATABASE_URL.split(':')[1],
            database=DATABASE_NAME,
            user=DATABASE_USER,
            password=DATABASE_PASSWORD)
        return conn
    except psycopg2.Error as e:
        print(f"Error connecting to database: {e}")
        return None

def create_table():
    conn = connect_to_db()
    if conn:
        cur = conn.cursor()
        cur.execute('''
            CREATE TABLE IF NOT EXISTS users (
                id SERIAL PRIMARY KEY,
                name TEXT NOT NULL,
                email TEXT NOT NULL
            )
        ''')
        conn.commit()
        cur.close()
        conn.close()

class UserList(Resource):
    def post(self):
        create_table()
        conn = connect_to_db()
        if not conn:
            return {'message': 'Database connection failed'}, 500

        data = request.get_json()
        name = data['name']
        email = data['email']

        cur = conn.cursor()
        cur.execute('INSERT INTO users (name, email) VALUES (%s, %s) RETURNING id, name, email', (name, email))
        user = cur.fetchone()
        conn.commit()
        cur.close()
        conn.close()

        return {'id': user[0], 'name': user[1], 'email': user[2]}, 201

    def get(self):
        create_table()
        conn = connect_to_db()
        if not conn:
            return {'message': 'Database connection failed'}, 500

        cur = conn.cursor()
        cur.execute('SELECT id, name, email FROM users')
        users = cur.fetchall()
        cur.close()
        conn.close()

        user_list = []
        for user in users:
            user_list.append({'id': user[0], 'name': user[1], 'email': user[2]})

        return user_list, 200

class User(Resource):
    def get(self, id):
        create_table()
        conn = connect_to_db()
        if not conn:
            return {'message': 'Database connection failed'}, 500

        cur = conn.cursor()
        cur.execute('SELECT id, name, email FROM users WHERE id = %s', (id,))
        user = cur.fetchone()
        cur.close()
        conn.close()

        if user:
            return {'id': user[0], 'name': user[1], 'email': user[2]}, 200
        else:
            return {'message': 'User not found'}, 404

    def put(self, id):
        create_table()
        conn = connect_to_db()
        if not conn:
            return {'message': 'Database connection failed'}, 500

        data = request.get_json()
        name = data['name']
        email = data['email']

        cur = conn.cursor()
        cur.execute('UPDATE users SET name = %s, email = %s WHERE id = %s', (name, email, id))
        conn.commit()

        if cur.rowcount == 0:
            cur.close()
            conn.close()
            return {'message': 'User not found'}, 404
        
        cur.close()
        conn.close()
        return '', 204

    def delete(self, id):
        create_table()
        conn = connect_to_db()
        if not conn:
            return {'message': 'Database connection failed'}, 500

        cur = conn.cursor()
        cur.execute('DELETE FROM users WHERE id = %s', (id,))
        conn.commit()

        if cur.rowcount == 0:
            cur.close()
            conn.close()
            return {'message': 'User not found'}, 404

        cur.close()
        conn.close()
        return '', 204

api.add_resource(UserList, '/users')
api.add_resource(User, '/users/<int:id>')

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=8080)
