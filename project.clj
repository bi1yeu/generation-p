(defproject generation-p "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/data.generators "0.1.2"]
                 [org.clojure/java.jdbc "0.7.10"]
                 [org.clojure/tools.logging "0.5.0"]
                 [org.slf4j/slf4j-log4j12 "1.7.12"]
                 [org.xerial/sqlite-jdbc "3.28.0"]
                 [twitter-api "1.8.0"]
                 ]
  :main ^:skip-aot generation-p.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
