(defsystem "complang-crud"
  :version "0.1.0"
  :author "You <you@example.com>"
  :license "MIT"
  :depends-on ((:cl-postgres) (:hunchentoot) (:cl-json))
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "db")
                 (:file "api")
                 (:file "models")))))
