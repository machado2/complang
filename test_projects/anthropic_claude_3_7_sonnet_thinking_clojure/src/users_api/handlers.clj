
(ns users-api.handlers
  (:require [users-api.db :as db]
            [ring.util.response :as resp]
            [clojure.string :as str]))

;; Parse ID from string to integer
(defn parse-id [id-str]
  (try
    (Integer/parseInt id-str)
    (catch NumberFormatException _
      nil)))

;; User handlers
(defn list-users [_]
  (resp/response (db/get-all-users)))

(defn get-user [request]
  (let [id (parse-id (get-in request [:params :id]))]
    (if-let [user (db/get-user-by-id id)]
      (resp/response user)
      (resp/not-found {:error (str "User " id " not found")}))))

(defn create-user [request]
  (let [user-data (:body request)
        created-user (db/create-user user-data)]
    (-> (resp/response created-user)
        (resp/status 201))))

(defn update-user [request]
  (let [id (parse-id (get-in request [:params :id]))
        user-data (:body request)]
    (if (db/update-user id user-data)
      (resp/response {:status "success"})
      (resp/not-found {:error (str "User " id " not found")}))))

(defn delete-user [request]
  (let [id (parse-id (get-in request [:params :id]))]
    (if (db/delete-user id)
      (resp/response {:status "success"})
      (resp/not-found {:error (str "User " id " not found")}))))
