
(ns app.core
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [compojure.core :refer [defroutes POST GET PUT DELETE routes]]
            [compojure.route :as route]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [clojure.java.jdbc :as jdbc]
            [environ.core :refer [env]]))

(def db-spec {:subprotocol "postgresql"
               :subname "//host.docker.internal:5432/complang"
               :user "testuser"
               :password (env :PGPASSWORD)})

(defn create-user [user]
  (let [result (jdbc/insert! db-spec :users user)]
    (first result)))

(defn get-users []
  (jdbc/query db-spec ["SELECT * FROM users"]))

(defn get-user [id]
  (jdbc/get-by-id db-spec :users id))

(defn update-user [id user]
  (jdbc/update! db-spec :users user ["id = ?" id]))

(defn delete-user [id]
  (jdbc/delete! db-spec :users ["id = ?" id]))

(defroutes app-routes
  (POST "/users" request
    (let [user (get-in request [:body])]
      (let [created-user (create-user user)]
        {:status 201
         :body created-user})))

  (GET "/users" []
    (let [users (get-users)]
      {:status 200
       :body users}))

  (GET "/users/:id" [id]
    (let [user (get-user (Integer. id))]
      (if user
        {:status 200
         :body user}
        {:status 404
         :body {:error "User not found"}})))

  (PUT "/users/:id" [id request]
    (let [user (get-in request [:body])]
      (if (update-user (Integer. id) user)
        {:status 204}
        {:status 404
         :body {:error "User not found"}})))

  (DELETE "/users/:id" [id]
    (if (delete-user (Integer. id))
      {:status 204}
      {:status 404
       :body {:error "User not found"}}))

  (route/not-found {:status 404 :body {:error "Not Found"}}))

(def app
  (-> app-routes
      wrap-json-body
      wrap-json-response))

(defn -main []
  (run-jetty app {:port 8080 :join? false}))
