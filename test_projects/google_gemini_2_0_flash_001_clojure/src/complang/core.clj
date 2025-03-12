
(ns complang.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults api-defaults]]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [clojure.java.jdbc :as jdbc]
            [cheshire.core :as json]
            [ring.util.response :as response]
            [clojure.string :as str])
  (:gen-class))

(def db-spec
  {:dbtype "postgresql"
   :host   "host.docker.internal"
   :port   5432
   :dbname "complang"
   :user   "testuser"
   :password (System/getenv "PGPASSWORD")})

(defn get-users []
  (try
    (jdbc/query db-spec "SELECT id, name, email FROM users")
    (catch Exception e
      (do
        (println "Error getting users:" (.getMessage e))
        [])))

(defn get-user [id]
  (try
    (first (jdbc/query db-spec ["SELECT id, name, email FROM users WHERE id = ?" id]))
    (catch Exception e
      (do
        (println "Error getting user:" (.getMessage e))
        nil)))

(defn create-user [user]
  (try
    (jdbc/insert! db-spec "users" user)
    (let [id (:id (first (jdbc/query db-spec "SELECT lastval() AS id")))]
      (get-user id))
    (catch Exception e
      (do
        (println "Error creating user:" (.getMessage e))
        nil)))

(defn update-user [id user]
  (let [existing-user (get-user id)]
    (if existing-user
      (try
        (jdbc/update! db-spec "users" user ["id = ?" id])
        (response/status (response/response "") 204) ; Return 204 No Content
      (catch Exception e
        (do
          (println "Error updating user:" (.getMessage e))
          (response/status (response/response "Update failed") 500)))
      (response/status (response/response "User not found") 404))))

(defn delete-user [id]
  (let [existing-user (get-user id)]
    (if existing-user
      (try
        (jdbc/delete! db-spec "users" ["id = ?" id])
        (response/status (response/response "") 204) ; Return 204 No Content
      (catch Exception e
        (do
          (println "Error deleting user:" (.getMessage e))
          (response/status (response/response "Delete failed") 500)))
      (response/status (response/response "User not found") 404))))

(defroutes app-routes
  (context "/users" []
    (GET "/" [] (json/encode (get-users)))
    (POST "/" {body :body}
      (let [user (json/decode (slurp body) true)
            new-user (create-user user)]
        (if new-user
          (response/status (response/created "/users/" (json/encode new-user)) 201)
          (response/status (response/response "Creation failed") 500))))
    (context "/:id" [id]
      (GET "/" []
        (let [user (get-user (Integer/parseInt id))]
          (if user
            (json/encode user)
            (response/status (response/response "User not found") 404))))
      (PUT "/" {body :body}
        (update-user (Integer/parseInt id) (json/decode (slurp body) true)))
      (DELETE "/" [] (delete-user (Integer/parseInt id)))))
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes (dissoc api-defaults :security)))

(defn -main [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "8080"))]
    (try
      (jetty/run-jetty app {:port port
                            :join? false})
      (catch Exception e
        (println "Failed to start Jetty:" (.getMessage e))))))
