
(defproject api "0.1.0-SNAPSHOT"
  :description "CRUD API for users"
  :url "http://example.com/api"
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [compojure "1.7.0"]
                 [ring/ring-defaults "0.3.4"]
                 [ring/ring-json "0.5.1"]
                 [ring/ring-jetty-adapter "1.9.6"]
                 [org.postgresql/postgresql "42.6.0"]
                 [com.github.seancorfield/next.jdbc "1.3.883"]]
  :main ^:skip-aot api.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                        :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
