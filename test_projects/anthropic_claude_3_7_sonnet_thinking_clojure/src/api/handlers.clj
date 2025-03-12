
(ns api.handlers
  (:require [compojure.core :refer [defroutes GET POST PUT DELETE]]
            [compojure.route :as route]
            [ring.util.response :as response]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [api.db :as db]))

;; Helper functions
(defn json-response [data status]
  (-> (response/response data)
      (response/status status)))

;; Handler functions
(defn get-all-users-handler [_]
  (json-response (db/get-all-users) 200))

(defn get-user-handler [id]
  (if-let [user (db/get-user-by-id (Integer/parseInt id))]
    (json-response user 200)
    (json-response {:error "User not found"} 404)))

(defn create-user-handler [req]
  (let [user-data (:body req)
        new-user (db/create-user user-data)]
    (json-response new-user 201)))

(defn update-user-handler [id req]
  (let [user-id (Integer/parseInt id)
        user-data (:body req)]
    (if-let [updated-user (db/update-user user-id user-data)]
      (json-response updated-user 200)
      (json-response {:error "User not found"} 404))))

(defn delete-user-handler [id]
  (let [user-id (Integer/parseInt id)]
    (if (db/delete-user user-id)
      (json-response {:message "User deleted"} 200)
      (json-response {:error "User not found"} 404))))

;; Routes
(defroutes app-routes
  (GET "/users" [] get-all-users-handler)
  (GET "/users/:id" [id] (fn [_] (get-user-handler id)))
  (POST "/users" [] create-user-handler)
  (PUT "/users/:id" [id] (fn [req] (update-user-handler id req)))
  (DELETE "/users/:id" [id] (fn [_] (delete-user-handler id)))
  (route/not-found (json-response {:error "Not found"} 404)))

;; Middleware
(def app-handler
  (-> app-routes
      (wrap-json-body {:keywords? true})
      wrap-json-response))
