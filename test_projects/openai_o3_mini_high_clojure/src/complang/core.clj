(ns complang.core
  (:require [compojure.core :refer [defroutes GET POST PUT DELETE]]
            [compojure.route :as route]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.json :as middleware]
            [ring.util.response :refer [response status]]
            [clojure.java.jdbc :as jdbc]))

;; Define the PostgreSQL database spec
(def db-spec
  {:subprotocol "postgresql"
   :subname "//host.docker.internal:5432/complang"
   :user "testuser"
   :password (System/getenv "PGPASSWORD")})

(defroutes app-routes
  ;; POST /users: Create a new user
  (POST "/users" {body :body}
    (try
      (let [result (jdbc/insert! db-spec :users (select-keys body [:name :email]))
            created-user (first result)]
        (-> (response created-user) (status 201)))
      (catch Exception e
        (-> (response {:error "Error creating user"}) (status 500)))))

  ;; GET /users: Retrieve all users
  (GET "/users" []
    (try
      (let [users (jdbc/query db-spec ["SELECT * FROM users"])]
        (response users))
      (catch Exception e
        (-> (response {:error "Error fetching users"}) (status 500)))))

  ;; GET /users/{id}: Retrieve a user by id
  (GET "/users/:id" [id]
    (try
      (let [user (first (jdbc/query db-spec ["SELECT * FROM users WHERE id = ?" (Integer/parseInt id)]))]
        (if user
          (response user)
          (-> (response {:error "User not found"}) (status 404))))
      (catch Exception e
        (-> (response {:error "Error fetching user"}) (status 500)))))

  ;; PUT /users/{id}: Update a user by id
  (PUT "/users/:id" [id :as {body :body}]
    (try
      (let [updated-count (jdbc/update! db-spec :users (select-keys body [:name :email]) ["id=?" (Integer/parseInt id)])]
        (if (pos? updated-count)
          (-> (response {:message "User updated successfully"}) (status 200))
          (-> (response {:error "User not found"}) (status 404))))
      (catch Exception e
        (-> (response {:error "Error updating user"}) (status 500)))))

  ;; DELETE /users/{id}: Delete a user by id
  (DELETE "/users/:id" [id]
    (try
      (let [deleted-count (jdbc/delete! db-spec :users ["id=?" (Integer/parseInt id)])]
        (if (pos? deleted-count)
          (-> (response {:message "User deleted successfully"}) (status 200))
          (-> (response {:error "User not found"}) (status 404))))
      (catch Exception e
        (-> (response {:error "Error deleting user"}) (status 500)))))

  (route/not-found "Not Found"))

(def app
  (-> app-routes
      (middleware/wrap-json-body {:keywords? true})
      middleware/wrap-json-response))

(defonce server (atom nil))

(defn start-server []
  (when @server
    (.stop @server))
  (reset! server (jetty/run-jetty app {:host "0.0.0.0" :port 8080 :join? false})))

(defn -main [& args]
  (println "Starting server on port 8080...")
  (start-server))
