(defproject com.maitria/specter-edn "0.1.0-SNAPSHOT"
  :description "Specter paths for working with formatted EDN and Clojure code"
  :url "http://github.com/eraserhd/specter-edn"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :deploy-repositories [["releases" :clojars]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.rpl/specter "0.11.1"]
                 [aysylu/loom "0.6.0"]
                 [rewrite-clj "0.4.12"]])
