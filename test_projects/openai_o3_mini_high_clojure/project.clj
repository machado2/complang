(defproject crud-api "0.1.0-SNAPSHOT"
  :description "CRUD API in Clojure"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [compojure "1.6.2"]
                 [ring/ring-defaults "0.3.2"]
                 [ring/ring-json "0.5.0"]
                 [ring/ring-jetty-adapter "1.9.4"]
                 [org.clojure/java.jdbc "0.7.12"]
                 [org.postgresql/postgresql "42.5.0"]]
  :main api.core
  :uberjar-name "crud-api.jar"
  :profiles {:uberjar {:aot :all}})