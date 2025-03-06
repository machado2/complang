(defsystem "crud-api"
  :name "crud-api"
  :version "0.1.0"
  :author "Your Name"
  :maintainer "your.email@example.com"
  :license "MIT"
  :depends-on (:cl-postgres
                :hunchentoot
                :yason
                :ppcre)
  :components ((:file "main.lisp")))
