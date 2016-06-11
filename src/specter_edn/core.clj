(ns specter-edn.core
  (:require [clojure.data.priority-map :as pm]
            [clojure.edn :as edn]
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
    (let [tree (p/parse-string-all source-code)]
      (->> (n/children tree)
        (map (fn [child]
               (if (n/printable-only? child)
                 child
                 (n/coerce (next-fn (n/sexpr child))))))
        (n/replace-children tree)
        n/string))))

(def SEXPRS (->SEXPRS-TYPE))
