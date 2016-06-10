(ns specter-edn.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [com.rpl.specter.protocols]
            [rewrite-clj.parser :as p]))

(deftype SEXPR-TYPE [])

(extend-protocol com.rpl.specter.protocols/Navigator
  SEXPR-TYPE
  (select* [_ source-code next-fn]
    (next-fn (edn/read-string source-code))))

(def SEXPR (->SEXPR-TYPE))
