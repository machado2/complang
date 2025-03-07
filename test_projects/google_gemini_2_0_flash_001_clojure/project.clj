
(defproject complang "0.1.0-SNAPSHOT"
  :description "CRUD API with Clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [ring/ring-core "1.9.6"]
                 [ring/ring-jetty-adapter "1.9.6"]
                 [compojure "1.7.0"]
                 [org.clojure/java.jdbc "0.7.12"]
                 [org.postgresql/postgresql "42.6.0"]
                 [environ "1.2.0"]
                 [ring.middleware.json "0.5.4"] ; Updated version
                 [cheshire "5.12.0"]]
  :main complang.core
  :aot :all
  :target-path "target/%s"
  :profiles {:default [:base :dev]
             :dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                                  [peridot "0.5.4"]]}
             :uberjar {:aot :all}}
  :plugins [[lein-ring "0.12.6"]
            [lein-environ "1.2.0"]])
