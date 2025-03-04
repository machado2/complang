
package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"strconv"

	"github.com/gorilla/mux"
	_ "github.com/lib/pq"
)

type User struct {
	ID    int    `json:"id"`
	Name  string `json:"name"`
	Email string `json:"email"`
}

var db *sql.DB

func main() {
	var err error
	dbinfo := fmt.Sprintf("host=%s port=%s user=%s password=%s dbname=%s sslmode=disable",
		"host.docker.internal", "5432", "postgres", os.Getenv("PGPASSWORD"), "test_google_gemini_2_0_flash_001_go")

	db, err = sql.Open("postgres", dbinfo)
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()

	err = db.Ping()
	if err != nil {
		log.Fatal(err)
	}

	// Check if the users table exists; create it if not.
	_, err = db.Query("SELECT id FROM users LIMIT 1")
	if err != nil {
		_, err = db.Exec(`
			CREATE TABLE users (
				id SERIAL PRIMARY KEY,
				name TEXT,
				email TEXT
			)
		`)
		if err != nil {
			log.Fatal(err)
		}
		fmt.Println("Created users table")
	}

	router := mux.NewRouter()
	router.HandleFunc("/users", CreateUser).Methods("POST")
	router.HandleFunc("/users", GetUsers).Methods("GET")
	router.HandleFunc("/users/{id}", GetUser).Methods("GET")
	router.HandleFunc("/users/{id}", UpdateUser).Methods("PUT")
	router.HandleFunc("/users/{id}", DeleteUser).Methods("DELETE")

	fmt.Println("Server listening on port 8080")
	log.Fatal(http.ListenAndServe(":8080", router))
}

func CreateUser(w http.ResponseWriter, r *http.Request) {
	body, _ := ioutil.ReadAll(r.Body)
	var user User

	err := json.Unmarshal(body, &user)
	if err != nil {
		http.Error(w, "Could not decode parameters", http.StatusBadRequest)
		return
	}

	err = validateUser(user)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	err = validateEmail(user.Email)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	err = validateName(user.Name)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	sqlStatement := `
		INSERT INTO users (name, email)
		VALUES ($1, $2)
		RETURNING id`
	id := 0
	err = db.QueryRow(sqlStatement, user.Name, user.Email).Scan(&id)
	if err != nil {
		log.Println("Error inserting new record: ", err)
		http.Error(w, "Failed to create user", http.StatusInternalServerError)
		return
	}
	user.ID = id

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusCreated)
	json.NewEncoder(w).Encode(user)
}

func GetUsers(w http.ResponseWriter, r *http.Request) {
	users := []User{}
	rows, err := db.Query("SELECT id, name, email FROM users")
	if err != nil {
		log.Println("Error selecting users: ", err)
		http.Error(w, "Failed to get users", http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	for rows.Next() {
		var user User
		err = rows.Scan(&user.ID, &user.Name, &user.Email)
		if err != nil {
			log.Println("Error scanning users: ", err)
			http.Error(w, "Failed to scan users", http.StatusInternalServerError)
			return
		}
		users = append(users, user)
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(users)
}

func GetUser(w http.ResponseWriter, r *http.Request) {
	params := mux.Vars(r)
	id, err := strconv.Atoi(params["id"])
	if err != nil {
		http.Error(w, "Invalid user ID", http.StatusBadRequest)
		return
	}

	var user User
	err = db.QueryRow("SELECT id, name, email FROM users WHERE id = $1", id).Scan(&user.ID, &user.Name, &user.Email)
	if err != nil {
		if err == sql.ErrNoRows {
			http.NotFound(w, r)
			return
		}
		http.Error(w, "Failed to get user", http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(user)
}

func UpdateUser(w http.ResponseWriter, r *http.Request) {
	params := mux.Vars(r)
	id, err := strconv.Atoi(params["id"])
	if err != nil {
		http.Error(w, "Invalid user ID", http.StatusBadRequest)
		return
	}

	body, _ := ioutil.ReadAll(r.Body)
	var user User
	err = json.Unmarshal(body, &user)

	if err != nil {
		http.Error(w, "Could not decode parameters", http.StatusBadRequest)
		return
	}

	err = validateUser(user)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	err = validateEmail(user.Email)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	err = validateName(user.Name)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	result, err := db.Exec("UPDATE users SET name = $1, email = $2 WHERE id = $3", user.Name, user.Email, id)
	if err != nil {
		http.Error(w, "Failed to update user", http.StatusInternalServerError)
		return
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		http.Error(w, "Failed to get affected rows", http.StatusInternalServerError)
		return
	}

	if rowsAffected == 0 {
		http.NotFound(w, r)
		return
	}

	w.WriteHeader(http.StatusNoContent)
}

func DeleteUser(w http.ResponseWriter, r *http.Request) {
	params := mux.Vars(r)
	id, err := strconv.Atoi(params["id"])
	if err != nil {
		http.Error(w, "Invalid user ID", http.StatusBadRequest)
		return
	}

	result, err := db.Exec("DELETE FROM users WHERE id = $1", id)
	if err != nil {
		http.Error(w, "Failed to delete user", http.StatusInternalServerError)
		return
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		http.Error(w, "Failed to get affected rows", http.StatusInternalServerError)
		return
	}

	if rowsAffected == 0 {
		http.NotFound(w, r)
		return
	}
	w.WriteHeader(http.StatusNoContent)
}

func validateUser(user User) error {
	if user.Name == "" {
		return fmt.Errorf("Name cannot be empty")
	}
	if user.Email == "" {
		return fmt.Errorf("Email cannot be empty")
	}
	return nil
}

func validateEmail(email string) error {
	if len(email) > 255 {
		return fmt.Errorf("email length exceeds maximum of 255 characters")
	}
	return nil
}

func validateName(name string) error {
	if len(name) > 255 {
		return fmt.Errorf("name length exceeds maximum of 255 characters")
	}
	return nil
}
