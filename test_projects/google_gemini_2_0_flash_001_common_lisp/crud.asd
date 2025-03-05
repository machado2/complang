
(asdf:defsystem "crud"
  :description "A simple CRUD API in Common Lisp"
  :author "Bard"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (:clack
               :alexandria
               :mito
               :sxql
               :postmodern
               :jonathan)
  :components ((:file "app")))
