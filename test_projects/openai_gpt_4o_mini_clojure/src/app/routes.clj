(ns app.routes
  (:require [ring.util.response :as response]
            [app.core :as core]))

(defn users-handler [request]
  (case (:request-method request)
    :get (if-let [id (-> request :path-params :id)]
            (let [user (core/get-user (Integer. id))]
              (if user
                (response/response user)
                (response/not-found "User not found")))
            (response/response (core/get-users)))
    :post (let [new-user (-> request :body)]
            (response/created (core/create-user new-user)))
    :put (let [id (-> request :path-params :id)
               user (-> request :body)]
            (if-let [updated-user (core/update-user (Integer. id) user)]
              (response/response updated-user)
              (response/not-found "User not found")))
    :delete (let [id (-> request :path-params :id)]
              (if (core/delete-user (Integer. id))
                (response/response "User deleted")
                (response/not-found "User not found")))))