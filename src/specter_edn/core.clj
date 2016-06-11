(ns specter-edn.core
  (:require [clojure.set :as set]
            [com.rpl.specter.protocols]
            [loom
             [alg :as a]
             [graph :as g]]
            [rewrite-clj
             [node :as n]
             [parser :as p]]))

(defn- jaccard-index
  "Compute the Jaccard similarity coefficient for two sets.

  https://en.wikipedia.org/wiki/Jaccard_index"
  [A B]
  (let [intersection-size (count (set/intersection A B))]
    (/ intersection-size (- (+ (count A) (count B)) intersection-size))))

(defn- change-weight
  "Compute the weight for changing node to represent sexpr.

  The idea here is that the more dissimilar node and sexpr are, the less we
  should want to make a change (as opposed to a delete and insert)."
  [node sexpr]
  (cond
    (and (not (n/printable-only? node)) (= (n/sexpr node) sexpr))
    0

    (and (n/inner? node) (coll? sexpr))
    (let [A (->> (n/child-sexprs node)
              flatten
              sort
              (into #{}))
          B (->> sexpr
              flatten
              sort
              (into #{}))]
      (- 1.0 (jaccard-index A B)))

    :else
    1))

(defn- update-plan-graph
  [nodes sexprs]
  {:pre [(vector? nodes)
         (vector? sexprs)]}
  (apply
    g/weighted-digraph
    (for [i (range (inc (count nodes)))
          j (range (inc (count sexprs)))
          [id jd] [[1 0] [0 1] [1 1]]
          :let [ii (+ i id)
                jj (+ j jd)
                weight (case [id jd]
                         [1 0]
                         (if-let [n (get nodes i)]
                           (if (n/printable-only? n)
                             0
                             1))

                         [0 1]
                         (if (get sexprs j)
                           1)

                         [1 1]
                         (if (and (get nodes i) (get sexprs j))
                           (change-weight (get nodes i) (get sexprs j))))]
          :when weight]
      [[i j] [ii jj] weight])))

(defn- find-update-plan
  [nodes sexprs]
  {:pre [(vector? nodes)
         (vector? sexprs)]}
  (->> (a/dijkstra-path (update-plan-graph nodes sexprs)
                        [0 0]
                        [(count nodes) (count sexprs)])
    (partition 2 1)
    (map (fn [[[i j] [ii jj]]]
           (let [id (- ii i)
                 jd (- jj j)]
             (case [id jd]
               [1 0] (if (n/printable-only? (get nodes i))
                       :keep
                       :remove)
               [0 1] :new
               [1 1] :match))))))

(defn- inner-node-ctor
  [node coll]
  (cond
    (= :forms (n/tag node)) n/forms-node
    (set? coll)             n/set-node
    (map? coll)             n/map-node
    (vector? coll)          n/vector-node
    (list? coll)            n/list-node
    :else                   (throw (Exception.
                                     (str "Not sure how to construct a node for "
                                          (pr-str coll)
                                          " (the original node had tag "
                                          (n/tag node)
                                          ")")))))

(defn- rebuild-inner-node
  [node coll children]
  ((inner-node-ctor node coll) children))

(defn- tree-update
  [node sexprs]
  {:pre [(not (n/printable-only? node))]}
  (cond
    (and (n/inner? node) (coll? sexprs))
    (->> (find-update-plan (vec (n/children node)) (vec sexprs))
      (reduce
        (fn [[output-nodes input-nodes input-sexprs] step]
          (case step
            :remove [output-nodes
                     (rest input-nodes)
                     input-sexprs]
            :keep   [(conj output-nodes (first input-nodes))
                     (rest input-nodes)
                     input-sexprs]
            :new    [(conj output-nodes (n/coerce (first input-sexprs)))
                     input-nodes
                     (rest input-sexprs)]
            :match  [(conj output-nodes (tree-update (first input-nodes) (first input-sexprs)))
                     (rest input-nodes)
                     (rest input-sexprs)]))
        [[] (n/children node) sexprs])
      first
      (rebuild-inner-node node sexprs))

    :else
    (n/coerce sexprs)))

(deftype SEXPRS-TYPE [])
(extend-protocol com.rpl.specter.protocols/Navigator
  SEXPRS-TYPE
  (select* [_ source-code next-fn]
    (let [tree (p/parse-string-all source-code)]
      (mapcat next-fn (n/child-sexprs tree))))
  (transform* [_ source-code next-fn]
    (let [tree (p/parse-string-all source-code)
          sexprs (vec (next-fn (n/child-sexprs tree)))
          new-tree (tree-update tree sexprs)]
      (n/string new-tree))))

(def SEXPRS (->SEXPRS-TYPE))
