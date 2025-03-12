
(ns complang
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.json :as ring-json]
            [ring.middleware.keyword-params :as ring-kp]
            [ring.middleware.params :as ring-params]
            [ring.util.response :as resp]
            [compojure.core :as compojure]
            [compojure.route :as route]))

(def db-spec
  {:classname   "org.postgresql.Driver"
   :subprotocol "postgresql"
   :subname     "//host.docker.internal:5432/complang"
   :user        "testuser"
   :password    (System/getenv "PGPASSWORD")})

(defn get-users []
  (jdbc/query db-spec "SELECT id, name, email FROM users"))

(defn get-user [id]
  (first (jdbc/query db-spec ["SELECT id, name, email FROM users WHERE id = ?" id])))

(defn create-user [user]
  (let [{:keys [name email]} user]
    (jdbc/insert! db-spec "users" {:name name :email email})))

(defn update-user [id user]
  (let [{:keys [name email]} user]
    (jdbc/update! db-spec "users" {:name name :email email} ["id = ?" id])))


(defn delete-user [id]
  (jdbc/delete! db-spec "users" ["id = ?" id]))

(compojure/defroutes app-routes
  (compojure/context "/users" []
    (compojure/GET "/" []
                 (resp/response (get-users)))
    (compojure/GET "/:id" [id]
                 (if-let [user (get-user (Integer/parseInt id))]
                   (resp/response user)
                   {:status 404}))
    (compojure/POST "/" {body :body}
                  (let [user (ring-json/parse-string body true)]
                    (create-user user)
                   (let [new-user (first (jdbc/query db-spec "SELECT id, name, email FROM users ORDER BY id DESC LIMIT 1"))]
                         (-> (resp/response new-user)
                           (resp/status 201)))))
    (compojure/PUT "/:id" {id :id body :body}
                 (let [user (ring-json/parse-string body true)
                       rows-updated (jdbc/update! db-spec "users" user ["id = ?" (Integer/parseInt id)])]
                   (if (> (first rows-updated) 0)
                     (resp/status 204)
                     {:status 404})))
    (compojure/DELETE "/:id" [id]
                 (let [rows-deleted (jdbc/delete! db-spec "users" ["id = ?" (Integer/parseInt id)])]
                   (if (> (first rows-deleted) 0)
                     (resp/status 204)
                     {:status 404}))))
  (route/not-found "Not Found"))

(def app
  (-> app-routes
      ring-json/wrap-json-body
      ring-json/wrap-json-response
      ring-kp/wrap-keyword-params
      ring-params/wrap-params))

(defn -main [& args]
  (jetty/run-jetty app {:port 8080 :join? false}))
