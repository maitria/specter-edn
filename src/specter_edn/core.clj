(ns specter-edn.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [com.rpl.specter.protocols]
            [rewrite-clj
             [node :as n]
             [parser :as p]]))

(deftype SEXPRS-TYPE [])
(extend-protocol com.rpl.specter.protocols/Navigator
  SEXPRS-TYPE
  (select* [_ source-code next-fn]
    (let [tree (p/parse-string-all source-code)]
      (mapcat next-fn (n/child-sexprs tree))))
  (transform* [_ source-code next-fn]
    (let [s-expr (next-fn (edn/read-string source-code))]
      (pr-str s-expr))))

(def SEXPRS (->SEXPRS-TYPE))
