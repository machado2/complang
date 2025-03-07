
(defproject users-api "0.1.0-SNAPSHOT"
  :description "A simple CRUD API for users"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [ring/ring-core "1.9.3"]
                 [ring/ring-json "0.5.0"]
                 [compojure "1.6.2"]
                 [mysql/mysql-connector-java "8.0.25"]
                 [clojure.java.jdbc "0.7.3"]
                 [org.postgresql/postgresql "42.2.23"]]
  :main app.core
  :resource-paths ["resources"]
  :profiles {:uberjar {:aot :all}})
