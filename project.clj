(defproject com.maitria/specter-edn "0.1.4"
  :description "Specter paths for working with formatted EDN and Clojure code"
  :url "http://github.com/eraserhd/specter-edn"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :deploy-repositories [["releases"
                         {:url "https://clojars.org/repo/"
                          :username :env/clojars_username
                          :password :env/clojars_password}]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [com.rpl/specter "1.0.1"]
                 [aysylu/loom "1.0.0"]
                 [rewrite-clj "0.6.0"]])
