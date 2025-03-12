
import unittest
import requests
import json

class TestCRUDAPI(unittest.TestCase):

    BASE_URL = "http://localhost:8080/users"

    def test_create_user(self):
        payload = {"name": "John Doe", "email": "john.doe@example.com"}
        response = requests.post(self.BASE_URL, data=json.dumps(payload))
        self.assertEqual(response.status_code, 201)
        user = response.json()
        self.assertEqual(user["name"], "John Doe")
        self.assertEqual(user["email"], "john.doe@example.com")
        self.assertTrue(isinstance(user["id"], int))
        # Store user_id for subsequent tests
        self.user_id = user["id"]

    def test_get_users(self):
        response = requests.get(self.BASE_URL)
        self.assertEqual(response.status_code, 200)
        users = response.json()
        self.assertTrue(isinstance(users, list))

    def test_get_user(self):
        # Assuming a user was created in test_create_user and self.user_id is set
        if hasattr(self, 'user_id'):
            response = requests.get(f"{self.BASE_URL}/{self.user_id}")
            self.assertEqual(response.status_code, 200)
            user = response.json()
            self.assertEqual(user["name"], "John Doe")
            self.assertEqual(user["email"], "john.doe@example.com")

    def test_update_user(self):
        # Assuming a user was created in test_create_user and self.user_id is set
        if hasattr(self, 'user_id'):
            payload = {"name": "Jane Doe", "email": "jane.doe@example.com"}
            response = requests.put(f"{self.BASE_URL}/{self.user_id}", data=json.dumps(payload))
            self.assertEqual(response.status_code, 204)

            # Verify the update
            response = requests.get(f"{self.BASE_URL}/{self.user_id}")
            self.assertEqual(response.status_code, 200)
            user = response.json()
            self.assertEqual(user["name"], "Jane Doe")
            self.assertEqual(user["email"], "jane.doe@example.com")

    def test_delete_user(self):
        # Assuming a user was created in test_create_user and self.user_id is set
        if hasattr(self, 'user_id'):
            response = requests.delete(f"{self.BASE_URL}/{self.user_id}")
            self.assertEqual(response.status_code, 204)

            # Verify the deletion
            response = requests.get(f"{self.BASE_URL}/{self.user_id}")
            self.assertEqual(response.status_code, 404)

if __name__ == '__main__':
    unittest.main()
