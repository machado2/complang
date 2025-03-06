(defproject task1 "0.1.0"
  :description "CRUD API"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License" :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"] ; Or a more recent version
                 [org.clojure/java.jdbc "0.7.12"]
                 [org.postgresql/postgresql "42.2.20"]
                 [ring/ring-core "1.9.6"]
                 [ring/ring-jetty-adapter "1.9.6"]
                 [compojure "1.7.0"] ; Or a more recent version
                 [cheshire "5.10.0"]]
  :main task1 :-main
  :aot :all
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                           :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
