(ns app.handler
  (:require [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [app.routes :as routes]))

(def app
  (-> routes/users-handler
      wrap-json-body
      wrap-json-response))