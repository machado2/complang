(ns app.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [next.jdbc :as jdbc]
            [clojure.java.jdbc :as j]]))

(def db-spec {:dbtype "postgres"
               :dbname "complang"
               :host "host.docker.internal"
               :port 5432
               :user "testuser"
               :password (System/getenv "PGPASSWORD")})

(defn create-user [user]
  (let [result (jdbc/insert! db-spec :users user)]
    (merge {:id (:id result)} user)))

(defn get-users []
  (jdbc/query db-spec ["SELECT * FROM users"]))

(defn get-user [id]
  (first (jdbc/query db-spec ["SELECT * FROM users WHERE id = ?" id])))

(defn update-user [id user]
  (jdbc/update! db-spec :users user ["id = ?" id])
  (jdbc/get-user id))

(defn delete-user [id]
  (jdbc/delete! db-spec :users ["id = ?" id]))

(def app
  (-> (jetty/create-default-handler)
      (wrap-json-body)
      (wrap-json-response)))