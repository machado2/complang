(defproject users "0.1.0-SNAPSHOT"
  :description "CRUD API for users"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0" :url "http://www.eclipse.org/legal/epl-v20.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [compojure/compojure "1.6.2"]
                 [ring/ring-jetty-adapter "1.9.6"]
                 [ring/ring-defaults "0.3.4"]
                 [ring/ring-json "0.5.1"]
                 [org.clojure/java.jdbc "0.7.12"]
                 [org.postgresql/postgresql "42.6.0"]
                 [clojure.data.json/clojure.data.json "2.4.0"]]
  :main ^:skip-aot users.main
  :target-path "target/%s"
  :profiles {:default   [:base :dev]
             :base      {:uberjar {:aot :all}}
             :production {}}
  :repl-options {:init-ns users.main})