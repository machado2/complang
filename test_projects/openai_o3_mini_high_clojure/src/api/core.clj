(ns api.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [clojure.java.jdbc :as jdbc])
  (:gen-class))

(def db-spec {:dbtype "postgresql"
              :host "host.docker.internal"
              :port 5432
              :dbname "complang"
              :user "testuser"
              :password (System/getenv "PGPASSWORD")})

(defn create-user [request]
  (let [{:keys [name email]} (:body request)]
    (if (and name email)
      (let [result (jdbc/with-db-transaction [tx db-spec]
                     (jdbc/insert! tx :users {:name name :email email} {:return-keys true}))
            user (first result)]
        {:status 201 :body user})
      {:status 400 :body {:error "Missing name or email"}})))

(defn list-users [request]
  (let [users (jdbc/query db-spec ["SELECT * FROM users"])]
    {:status 200 :body users}))

(defn get-user [id]
  (let [id (Integer/parseInt id)
        user (first (jdbc/query db-spec ["SELECT * FROM users WHERE id = ?" id]))]
    (if user
      {:status 200 :body user}
      {:status 404 :body {:error "User not found"}})))

(defn update-user [id request]
  (let [{:keys [name email]} (:body request)
        id (Integer/parseInt id)]
    (if (and name email)
      (let [result (jdbc/update! db-spec :users {:name name :email email} ["id = ?" id])
            row-count (if (sequential? result) (first result) result)]
        (if (pos? row-count)
          {:status 200 :body {:id id :name name :email email}}
          {:status 404 :body {:error "User not found"}}))
      {:status 400 :body {:error "Missing name or email"}})))

(defn delete-user [id]
  (let [id (Integer/parseInt id)
        result (jdbc/delete! db-spec :users ["id = ?" id])
        row-count (if (sequential? result) (first result) result)]
    (if (pos? row-count)
      {:status 200 :body {:message "User deleted"}}
      {:status 404 :body {:error "User not found"}})))

(defroutes app-routes
  (POST "/users" request (create-user request))
  (GET "/users" request (list-users request))
  (GET "/users/:id" [id] (get-user id))
  (PUT "/users/:id" [id :as request] (update-user id request))
  (DELETE "/users/:id" [id] (delete-user id))
  (route/not-found {:error "Not found"}))

(def app
  (-> app-routes
      (wrap-json-body {:keywords? true})
      wrap-json-response))

(defn -main [& args]
  (println "Starting server on port 8080")
  (run-jetty app {:port 8080 :join? false}))