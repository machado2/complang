package main

import (
    "database/sql"
    "encoding/json"
    "fmt"
    "log"
    "net/http"
    "os"
    "strconv"

    _ "github.com/lib/pq"
    "github.com/gorilla/mux"
)

var db *sql.DB

type User struct {
    ID int `json:"id"`
    Name string `json:"name"`
    Email string `json:"email"`
}

func main() {
    pgPassword := os.Getenv("PGPASSWORD")
    if pgPassword == "" {
        log.Fatal("PGPASSWORD environment variable is not set")
    }

    connStr := fmt.Sprintf("host=host.docker.internal port=5432 dbname=complang user=testuser password=%s sslmode=disable", pgPassword)
    var err error
    db, err = sql.Open("postgres", connStr)
    if err != nil {
        log.Fatal(err)
    }
    if err = db.Ping(); err != nil {
        log.Fatal(err)
    }

    router := mux.NewRouter()
    router.HandleFunc("/users", createUser).Methods("POST")
    router.HandleFunc("/users", getUsers).Methods("GET")
    router.HandleFunc("/users/{id}", getUser).Methods("GET")
    router.HandleFunc("/users/{id}", updateUser).Methods("PUT")
    router.HandleFunc("/users/{id}", deleteUser).Methods("DELETE")

    log.Println("Server is running on port 8080")
    log.Fatal(http.ListenAndServe(":8080", router))
}

func createUser(w http.ResponseWriter, r *http.Request) {
    var user User
    if err := json.NewDecoder(r.Body).Decode(&user); err != nil {
        http.Error(w, err.Error(), http.StatusBadRequest)
        return
    }

    query := "INSERT INTO users (name, email) VALUES($1, $2) RETURNING id"
    if err := db.QueryRow(query, user.Name, user.Email).Scan(&user.ID); err != nil {
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }

    w.Header().Set("Content-Type", "application/json")
    w.WriteHeader(http.StatusCreated)
    json.NewEncoder(w).Encode(user)
}

func getUsers(w http.ResponseWriter, r *http.Request) {
    rows, err := db.Query("SELECT id, name, email FROM users")
    if err != nil {
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }
    defer rows.Close()

    var users []User
    for rows.Next() {
        var user User
        if err := rows.Scan(&user.ID, &user.Name, &user.Email); err != nil {
            http.Error(w, err.Error(), http.StatusInternalServerError)
            return
        }
        users = append(users, user)
    }

    w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(users)
}

func getUser(w http.ResponseWriter, r *http.Request) {
    vars := mux.Vars(r)
    id, err := strconv.Atoi(vars["id"])
    if err != nil {
        http.Error(w, "Invalid user ID", http.StatusBadRequest)
        return
    }

    var user User
    err = db.QueryRow("SELECT id, name, email FROM users WHERE id = $1", id).Scan(&user.ID, &user.Name, &user.Email)
    if err != nil {
        if err == sql.ErrNoRows {
            http.Error(w, "User not found", http.StatusNotFound)
            return
        }
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }

    w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(user)
}

func updateUser(w http.ResponseWriter, r *http.Request) {
    vars := mux.Vars(r)
    id, err := strconv.Atoi(vars["id"])
    if err != nil {
        http.Error(w, "Invalid user ID", http.StatusBadRequest)
        return
    }

    var user User
    if err := json.NewDecoder(r.Body).Decode(&user); err != nil {
        http.Error(w, err.Error(), http.StatusBadRequest)
        return
    }

    res, err := db.Exec("UPDATE users SET name = $1, email = $2 WHERE id = $3", user.Name, user.Email, id)
    if err != nil {
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }
    if count, _ := res.RowsAffected(); count == 0 {
        http.Error(w, "User not found", http.StatusNotFound)
        return
    }
    w.WriteHeader(http.StatusOK)
}

func deleteUser(w http.ResponseWriter, r *http.Request) {
    vars := mux.Vars(r)
    id, err := strconv.Atoi(vars["id"])
    if err != nil {
        http.Error(w, "Invalid user ID", http.StatusBadRequest)
        return
    }

    res, err := db.Exec("DELETE FROM users WHERE id = $1", id)
    if err != nil {
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }
    if count, _ := res.RowsAffected(); count == 0 {
        http.Error(w, "User not found", http.StatusNotFound)
        return
    }
    w.WriteHeader(http.StatusOK)
}