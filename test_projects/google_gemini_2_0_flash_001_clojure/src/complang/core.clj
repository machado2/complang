
(ns complang.core
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [clojure.java.jdbc :as jdbc]
            [environ.core :refer [env]]
            [ring.middleware.json :as middleware]
            [ring.util.response :as response]
            [cheshire.core :as json]
            [ring.middleware.params :as params]))

(def db-spec
  {:dbtype "postgresql"
   :host "host.docker.internal"
   :port 5432
   :dbname "complang"
   :user "testuser"
   :password (env :pgpassword)})

(defn get-users []
  (jdbc/query db-spec "SELECT id, name, email FROM users"))

(defn get-user [id]
  (first (jdbc/query db-spec ["SELECT id, name, email FROM users WHERE id = ?" id]))

(defn create-user [user]
  (let [result (jdbc/insert! db-spec :users user)]
    (get-user (:id (first result)))))

(defn update-user [id user]
  (let [result (jdbc/update! db-spec :users user ["id = ?" id])]
    (if (= 1 result)
      (get-user id)
      nil)))

(defn delete-user [id]
  (let [result (jdbc/delete! db-spec :users ["id = ?" id])]
    (= 1 result)))

(defroutes app-routes
  (context "/users" []
    (GET "/" []
      (response/response (get-users)))
    (POST "/" []
      (middleware/wrap-json-body
       (fn [request]
         (let [user (create-user (:body request))]
           (if user
             (-> (response/response user)
                 (response/status 201))
             (response/not-found "Could not create user"))))))
    (context "/:id" [id]
      (GET "/" []
        (let [id (Integer/parseInt id)
              user (get-user id)]
          (if user
            (response/response user)
            (response/not-found "User not found"))))
      (PUT "/" []
        (middleware/wrap-json-body
         (fn [request]
           (let [id (Integer/parseInt id)
                 user (update-user id (:body request))]
             (if user
               (response/response user)
               (response/not-found "User not found"))))))
      (DELETE "/" []
        (let [id (Integer/parseInt id)
              deleted (delete-user id)]
          (if deleted
            (response/status 204)
            (response/not-found "User not found")))))))
  (route/not-found "Not Found"))

(def app
  (-> app-routes
      (params/wrap-params)
      (middleware/wrap-json-response)
      ))

(defn -main [& args]
  (let [port (Integer/parseInt (or (env :port) "8080"))]
    (jetty/run-jetty app {:port port :join? false})))
