
package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"net/http"

	_ "github.com/lib/pq"
)

var db *sql.DB

func main() {
	// Load environment variables
	pgpwd := "Saloon5-Moody-Observing"

	// Connect to the database
	var err error
	db, err = sql.Open("postgres", fmt.Sprintf("host=host.docker.internal port=5432 user=testuser password=%s dbname=complang sslmode=disable", pgpwd))
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()

	// Start the HTTP server
	http.HandleFunc("/users", usersHandler)
	http.HandleFunc("/users/", userHandler)
	log.Fatal(http.ListenAndServe(":8080", nil))
}

func usersHandler(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case "POST":
		createUser(w, r)
	case "GET":
		getUsers(w, r)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

func userHandler(w http.ResponseWriter, r *http.Request) {
	// Get the user ID from the URL
	userID := r.URL.Path[len("/users/"):]

	switch r.Method {
	case "GET":
		getUser(w, r, userID)
	case "PUT":
		updateUser(w, r, userID)
	case "DELETE":
		deleteUser(w, r, userID)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

func createUser(w http.ResponseWriter, r *http.Request) {
	var user struct {
		Name  string `json:"name"`
		Email string `json:"email"`
	}
	err := json.NewDecoder(r.Body).Decode(&user)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	// Insert the user into the database
	result, err := db.Exec("INSERT INTO users (name, email) VALUES ($1, $2)", user.Name, user.Email)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Get the ID of the newly inserted user
	userID, err := result.LastInsertId()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Return the created user
	w.WriteHeader(http.StatusCreated)
	json.NewEncoder(w).Encode(struct {
		ID    int    `json:"id"`
		Name  string `json:"name"`
		Email string `json:"email"`
	}{
		ID:    int(userID),
		Name:  user.Name,
		Email: user.Email,
	})
}

func getUsers(w http.ResponseWriter, r *http.Request) {
	// Query the database for all users
	rows, err := db.Query("SELECT id, name, email FROM users")
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	// Create a slice to store the users
	var users []struct {
		ID    int    `json:"id"`
		Name  string `json:"name"`
		Email string `json:"email"`
	}

	// Iterate over the rows and append the users to the slice
	for rows.Next() {
		var user struct {
			ID    int    `json:"id"`
			Name  string `json:"name"`
			Email string `json:"email"`
		}
		err = rows.Scan(&user.ID, &user.Name, &user.Email)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		users = append(users, user)
	}

	// Return the list of users
	json.NewEncoder(w).Encode(users)
}

func getUser(w http.ResponseWriter, r *http.Request, userID string) {
	// Query the database for the user
	var user struct {
		ID    int    `json:"id"`
		Name  string `json:"name"`
		Email string `json:"email"`
	}
	err := db.QueryRow("SELECT id, name, email FROM users WHERE id = $1", userID).Scan(&user.ID, &user.Name, &user.Email)
	if err != nil {
		if err == sql.ErrNoRows {
			http.Error(w, "User not found", http.StatusNotFound)
		} else {
			http.Error(w, err.Error(), http.StatusInternalServerError)
		}
		return
	}

	// Return the user
	json.NewEncoder(w).Encode(user)
}

func updateUser(w http.ResponseWriter, r *http.Request, userID string) {
	var user struct {
		Name  string `json:"name"`
		Email string `json:"email"`
	}
	err := json.NewDecoder(r.Body).Decode(&user)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	// Update the user in the database
	result, err := db.Exec("UPDATE users SET name = $1, email = $2 WHERE id = $3", user.Name, user.Email, userID)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Check if the user was updated
	rowsAffected, err := result.RowsAffected()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	if rowsAffected == 0 {
		http.Error(w, "User not found", http.StatusNotFound)
		return
	}

	// Return a 200 status code
	w.WriteHeader(http.StatusOK)
}

func deleteUser(w http.ResponseWriter, r *http.Request, userID string) {
	// Delete the user from the database
	result, err := db.Exec("DELETE FROM users WHERE id = $1", userID)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Check if the user was deleted
	rowsAffected, err := result.RowsAffected()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	if rowsAffected == 0 {
		http.Error(w, "User not found", http.StatusNotFound)
		return
	}

	// Return a 204 status code
	w.WriteHeader(http.StatusNoContent)
}
