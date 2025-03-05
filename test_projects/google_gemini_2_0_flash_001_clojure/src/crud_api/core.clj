
(ns crud-api.core
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :as comp]
            [compojure.route :as route]
            [cheshire.core :as json]
            [clojure.java.jdbc :as jdbc]
            [ring.middleware.json :as middleware]
            [ring.util.response :as response])
  (:gen-class))

(def db-spec
  {:dbtype "postgresql"
   :host "host.docker.internal"
   :port 5432
   :dbname "test_google_gemini_2_0_flash_001_clojure"
   :user "postgres"
   :password (System/getenv "PGPASSWORD")})

(defn create-table-if-not-exists []
  (try
    (jdbc/db-do-commands db-spec
                         (jdbc/create-table-ddl
                          :users
                          [[:id :serial :primary :key]
                           [:name :text]
                           [:email :text]]))
    (catch Exception e
      (println "Table already exists or other error: " (.getMessage e)))))

(defn get-users []
  (jdbc/query db-spec "SELECT * FROM users"))

(defn get-user [id]
  (first (jdbc/query db-spec ["SELECT * FROM users WHERE id = ?" id])))

(defn create-user [user]
  (try
    (let [id (:id (first (jdbc/insert! db-spec :users user :return-keys [:id])))]
      (assoc user :id id))
  (catch Exception e
    (println "Error creating user: " (.getMessage e))
    nil))

(defn update-user [id user]
  (let [result (jdbc/update! db-spec :users user ["id = ?" id])]
    (if (= 1 result)
      {:status 200}
      {:status 404})))

(defn delete-user [id user]
  (let [result (jdbc/delete! db-spec :users ["id = ?" id])]
    (if (= 1 result)
      {:status 200}
      {:status 404})))

(comp/defroutes app-routes
  (comp/context "/users" []
    (comp/GET "/" []
         (response/response (get-users)))
    (comp/POST "/" {body :body}
         (let [user (json/decode (slurp body) true)
               created-user (create-user user)]
           (if created-user
             (response/created "/users" created-user)
             {:status 500})))
    (comp/context "/:id" [id]
      (comp/GET "/" []
           (let [user (get-user (Integer/parseInt id))]
             (if user
               (response/response user)
               {:status 404})))
      (comp/PUT "/" {body :body}
           (let [user (json/decode (slurp body) true)
                 result (update-user (Integer/parseInt id) user)]
             (if (= 1 result)
               {:status 204}
               {:status 404})))
      (comp/DELETE "/" []
           (let [result (delete-user (Integer/parseInt id))]
             (if (= 1 result)
               {:status 204}
               {:status 404}))))))

  (route/not-found "Not Found"))

(def app
  (-> app-routes
      middleware/wrap-json-body
      middleware/wrap-json-response))

(defn -main [& args]
  (create-table-if-not-exists)
  (let [handler (middleware/wrap-json-body (middleware/wrap-json-response app-routes))]
  (jetty/run-jetty handler {:port 8080 :join? false})))
