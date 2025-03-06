(ns users.main
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.json :as middleware]
            [ring.middleware.params :as params]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [clojure.java.jdbc :as jdbc]
            [clojure.data.json :as json]
            [clojure.string :as str]))

(def db-spec
  {:dbtype "postgresql"
   :host   "host.docker.internal"
   :port   5432
   :dbname "complang"
   :user   "testuser"
   :password (System/getenv "PGPASSWORD")})

(defn db-query [query params]
  (try
    (jdbc/query db-spec query params)
    (catch Exception e
      (println "Database error:" (.getMessage e))
      nil)))

(defn db-execute [query params]
  (try
    (jdbc/execute! db-spec query params)
    (catch Exception e
      (println "Database error:" (.getMessage e))
      nil)))

(defn user->json [user]
  {:id (:id user)
   :name (:name user)
   :email (:email user)})

(defn create-user [params]
  (let [{:keys [name email]} params
        result (db-execute! db-spec
                             ["INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email"] [name email])]
    (if (seq result)
      (first (map user->json result))
      nil)))

(defn get-users []
  (map user->json (db-query "SELECT id, name, email FROM users" [])))

(defn get-user [id]
  (first (map user->json (db-query "SELECT id, name, email FROM users WHERE id = ?" [id]))))

(defn update-user [id params]
  (let [{:keys [name email]} params]
   (db-execute! db-spec ["UPDATE users SET name = ?, email = ? WHERE id = ?"] [name email id])
   (get-user id)))

(defn delete-user [id]
  (db-execute! db-spec ["DELETE FROM users WHERE id = ?"] [id]))

(defroutes app-routes
  (context "/users" []
    (POST "/" {params :params} 
          (let [new-user (create-user params)]
            (if new-user
              {:status 201
               :headers {"Content-Type" "application/json"}
               :body new-user}
              {:status 500
               :body "Failed to create user"})))
    (GET "/" []
         {:status 200
          :headers {"Content-Type" "application/json"}
          :body (get-users)})
    (context "/:id" [id :id]
      (GET "/" []
           (let [user (get-user (Integer/parseInt id))]
             (if user
               {:status 200
                :headers {"Content-Type" "application/json"}
                :body user}
               {:status 404
                :body "User not found"})))
      (PUT "/" {params :params} 
           (let [updated-user (update-user (Integer/parseInt id) params)]
             (if updated-user
               {:status 200
                :headers {"Content-Type" "application/json"}
                :body updated-user}
               {:status 404
                :body "User not found"})))
      (DELETE "/" []
              (let [deleted (delete-user (Integer/parseInt id))]
               (if deleted
                 {:status 204}
                 {:status 404
                  :body "User not found"})))))
    (route/not-found "Not Found")))

(def app
  (-> app-routes
      (params/wrap-params)
      (middleware/wrap-json-body {:keywords? true})
      (middleware/wrap-json-response)))

(defn -main [& args]
  (jetty/run-jetty app {:port 8080 :join? false}))