
(ns api.db
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def db-spec
  {:dbtype "postgresql"
   :dbname "complang"
   :host "host.docker.internal"
   :port 5432
   :user "testuser"
   :password (System/getenv "PGPASSWORD")})

(def ds (jdbc/get-datasource db-spec))

(defn get-all-users []
  (jdbc/execute! ds ["SELECT * FROM users"]
                 {:builder-fn rs/as-unqualified-maps}))

(defn get-user-by-id [id]
  (first (jdbc/execute! ds ["SELECT * FROM users WHERE id = ?" id]
                       {:builder-fn rs/as-unqualified-maps})))

(defn create-user [user]
  (first (jdbc/execute! ds ["INSERT INTO users (name, email) VALUES (?, ?) RETURNING *"
                          (:name user) (:email user)]
                       {:builder-fn rs/as-unqualified-maps})))

(defn update-user [id user]
  (let [result (jdbc/execute! ds ["UPDATE users SET name = ?, email = ? WHERE id = ? RETURNING *"
                               (:name user) (:email user) id]
                            {:builder-fn rs/as-unqualified-maps})]
    (first result)))

(defn delete-user [id]
  (pos? (first (jdbc/execute-one! ds ["DELETE FROM users WHERE id = ?" id]))))
