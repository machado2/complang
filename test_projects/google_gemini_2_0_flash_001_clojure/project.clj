
(defproject crud-api "0.1.0-SNAPSHOT"
  :description "CRUD API with Clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [ring/ring-core "1.9.4"]
                 [ring/ring-jetty-adapter "1.9.4"]
                 [compojure "1.6.2"]
                 [cheshire "5.10.0"]
                 [org.clojure/java.jdbc "0.7.12"]
                 [org.postgresql/postgresql "42.6.0"]
                 [ring/ring-json "0.5.1"]]
  :main crud-api.core
  :aot :all
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
