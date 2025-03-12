package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"

	"github.com/gorilla/mux"
	_ "github.com/lib/pq"
)

// User represents a user in our system
type User struct {
	ID    int    `json:"id,omitempty"`
	Name  string `json:"name"`
	Email string `json:"email"`
}

// App encapsulates the application
type App struct {
	Router *mux.Router
	DB     *sql.DB
}

// Initialize sets up the database connection and router
func (a *App) Initialize() {
	// Get password from environment variable
	password := os.Getenv("PGPASSWORD")
	if password == "" {
		log.Fatal("PGPASSWORD environment variable not set")
	}

	// Connection string for PostgreSQL
	connectionString := fmt.Sprintf("host=host.docker.internal port=5432 user=testuser password=%s dbname=complang sslmode=disable", password)

	var err error
	a.DB, err = sql.Open("postgres", connectionString)
	if err != nil {
		log.Fatal(err)
	}

	// Test the connection
	err = a.DB.Ping()
	if err != nil {
		log.Fatal(err)
	}

	a.Router = mux.NewRouter()
	a.setupRoutes()
}

// setupRoutes defines all the routes for our application
func (a *App) setupRoutes() {
	a.Router.HandleFunc("/users", a.getUsers).Methods("GET")
	a.Router.HandleFunc("/users", a.createUser).Methods("POST")
	a.Router.HandleFunc("/users/{id}", a.getUser).Methods("GET")
	a.Router.HandleFunc("/users/{id}", a.updateUser).Methods("PUT")
	a.Router.HandleFunc("/users/{id}", a.deleteUser).Methods("DELETE")
}

// Run starts the application
func (a *App) Run(addr string) {
	log.Printf("Server starting on %s", addr)
	log.Fatal(http.ListenAndServe(addr, a.Router))
}

// getUsers returns all users
func (a *App) getUsers(w http.ResponseWriter, r *http.Request) {
	users, err := getUsers(a.DB)
	if err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, users)
}

// getUser returns a specific user by ID
func (a *App) getUser(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id, err := strconv.Atoi(vars["id"])
	if err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid user ID")
		return
	}

	u := User{ID: id}
	if err := u.getUser(a.DB); err != nil {
		switch err {
		case sql.ErrNoRows:
			respondWithError(w, http.StatusNotFound, "User not found")
		default:
			respondWithError(w, http.StatusInternalServerError, err.Error())
		}
		return
	}

	respondWithJSON(w, http.StatusOK, u)
}

// createUser creates a new user
func (a *App) createUser(w http.ResponseWriter, r *http.Request) {
	var u User
	decoder := json.NewDecoder(r.Body)
	if err := decoder.Decode(&u); err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid request payload")
		return
	}
	defer r.Body.Close()

	if err := u.createUser(a.DB); err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusCreated, u)
}

// updateUser updates an existing user
func (a *App) updateUser(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id, err := strconv.Atoi(vars["id"])
	if err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid user ID")
		return
	}

	var u User
	decoder := json.NewDecoder(r.Body)
	if err := decoder.Decode(&u); err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid request payload")
		return
	}
	defer r.Body.Close()
	u.ID = id

	// Check if user exists
	existing := User{ID: id}
	if err := existing.getUser(a.DB); err != nil {
		switch err {
		case sql.ErrNoRows:
			respondWithError(w, http.StatusNotFound, "User not found")
		default:
			respondWithError(w, http.StatusInternalServerError, err.Error())
		}
		return
	}

	if err := u.updateUser(a.DB); err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, u)
}

// deleteUser deletes a user
func (a *App) deleteUser(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id, err := strconv.Atoi(vars["id"])
	if err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid user ID")
		return
	}

	u := User{ID: id}
	
	// Check if user exists
	if err := u.getUser(a.DB); err != nil {
		switch err {
		case sql.ErrNoRows:
			respondWithError(w, http.StatusNotFound, "User not found")
		default:
			respondWithError(w, http.StatusInternalServerError, err.Error())
		}
		return
	}
	
	if err := u.deleteUser(a.DB); err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, map[string]string{"result": "success"})
}

// User methods for database operations

func (u *User) getUser(db *sql.DB) error {
	return db.QueryRow("SELECT name, email FROM users WHERE id=$1", u.ID).Scan(&u.Name, &u.Email)
}

func (u *User) updateUser(db *sql.DB) error {
	_, err := db.Exec("UPDATE users SET name=$1, email=$2 WHERE id=$3", u.Name, u.Email, u.ID)
	return err
}

func (u *User) deleteUser(db *sql.DB) error {
	_, err := db.Exec("DELETE FROM users WHERE id=$1", u.ID)
	return err
}

func (u *User) createUser(db *sql.DB) error {
	err := db.QueryRow("INSERT INTO users(name, email) VALUES($1, $2) RETURNING id", u.Name, u.Email).Scan(&u.ID)
	return err
}

func getUsers(db *sql.DB) ([]User, error) {
	rows, err := db.Query("SELECT id, name, email FROM users")
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	users := []User{}
	for rows.Next() {
		var u User
		if err := rows.Scan(&u.ID, &u.Name, &u.Email); err != nil {
			return nil, err
		}
		users = append(users, u)
	}

	return users, nil
}

// Helper functions for HTTP responses

func respondWithError(w http.ResponseWriter, code int, message string) {
	respondWithJSON(w, code, map[string]string{"error": message})
}

func respondWithJSON(w http.ResponseWriter, code int, payload interface{}) {
	response, _ := json.Marshal(payload)
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(code)
	w.Write(response)
}

func main() {
	a := App{}
	a.Initialize()
	a.Run(":8080")
}
