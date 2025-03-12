(asdf:defsystem "user-api"
  :description "User API CRUD application"
  :version "0.1.0"
  :depends-on (:hunchentoot :postmodern :cl-json)
  :components ((:file "app")))
