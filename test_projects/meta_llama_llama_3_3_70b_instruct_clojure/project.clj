(defproject complang "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.11.1"]
                   [compojure "1.6.2"]
                   [ring/ring-jetty-adapter "1.9.6"]
                   [seancorfield/next-jdbc "1.3.829"]]
  :main complang.core)