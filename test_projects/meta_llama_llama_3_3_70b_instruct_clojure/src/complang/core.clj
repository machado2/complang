(ns complang.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.json :refer [wrap-json-body]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [next.jdbc.sql :refer [query]]))

(def db {:jdbc-url (str "jdbc:postgresql://host.docker.internal:5432/complang")
             :user "testuser"
             :password (System/getenv "PGPASSWORD")})

(defn get-users []
  (jdbc/execute! db ["SELECT * FROM users"]))

(defn get-user [id]
  (jdbc/execute! db [(str "SELECT * FROM users WHERE id = " id)]))

(defn create-user [name email]
  (jdbc/execute! db ["INSERT INTO users (name, email) VALUES (?,?) RETURNING *"
                 name email]))

(defn update-user [id name email]
  (jdbc/execute! db [(str "UPDATE users SET name =?, email =? WHERE id =? RETURNING *")
                 name email id]))

(defn delete-user [id]
  (jdbc/execute! db [(str "DELETE FROM users WHERE id =?")
                 id]))

(defroutes app
  (GET "/users" [] (get-users))
  (GET "/users/:id" [id] (get-user id))
  (POST "/users" {body :body} (create-user (:name body) (:email body)))
  (PUT "/users/:id" [id :as {body :body}] (update-user id (:name body) (:email body)))
  (DELETE "/users/:id" [id] (delete-user id))
  (route/not-found "Not found"))