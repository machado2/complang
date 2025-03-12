
(defproject complang "0.1.0-SNAPSHOT"
  :description "CRUD API for users"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [ring/ring-core "1.9.6"]
                 [ring/ring-jetty-adapter "1.9.6"]
                 [ring/ring-defaults "0.3.4"]
                 [compojure "1.7.0"]
                 [org.clojure/java.jdbc "0.7.12"]
                 [org.postgresql/postgresql "42.6.0"]
                 [cheshire "5.12.0"]]
  :main ^:skip-aot complang.core
  :target-path "target/%s"
  :profiles {:default       [:base :dev]
             :dev           {:dependencies [[javax.servlet/servlet-api "2.5"]
                                           [ring-mock "0.1.5"]]}
             :uberjar       {:aot :all
                             :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :min-lein-version "2.0.0")
