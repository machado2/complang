
(ns users-api.db
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn get-db-config []
  {:dbtype "postgresql"
   :dbname "complang"
   :host "host.docker.internal"
   :port 5432
   :user "testuser"
   :password (System/getenv "PGPASSWORD")})

(defn get-datasource []
  (jdbc/get-datasource (get-db-config)))

;; User CRUD operations
(defn get-all-users []
  (jdbc/execute! (get-datasource)
                ["SELECT * FROM users"]
                {:builder-fn rs/as-unqualified-maps}))

(defn get-user-by-id [id]
  (first (jdbc/execute! (get-datasource)
                       ["SELECT * FROM users WHERE id = ?" id]
                       {:builder-fn rs/as-unqualified-maps})))

(defn create-user [user]
  (jdbc/execute-one! (get-datasource)
                    ["INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email"
                     (:name user) (:email user)]
                    {:builder-fn rs/as-unqualified-maps}))

(defn update-user [id user]
  (let [result (jdbc/execute-one! (get-datasource)
                                 ["UPDATE users SET name = ?, email = ? WHERE id = ? RETURNING id"
                                  (:name user) (:email user) id]
                                 {:builder-fn rs/as-unqualified-maps})]
    (if (:id result) true false)))

(defn delete-user [id]
  (let [result (jdbc/execute-one! (get-datasource)
                                 ["DELETE FROM users WHERE id = ? RETURNING id" id]
                                 {:builder-fn rs/as-unqualified-maps})]
    (if (:id result) true false)))
