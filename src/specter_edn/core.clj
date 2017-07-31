(ns specter-edn.core
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :as walk]
            [clojure.set :as set]
            [com.rpl.specter :as specter :refer [defnav]]
            [loom
             [alg :as a]
             [graph :as g]]
            [rewrite-clj.node.whitespace :as w]
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
              (into #{}))
          B (->> sexpr
              flatten
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
                         (when-let [n (get nodes i)]
                           (if (n/printable-only? n)
                             0
                             1))

                         [0 1]
                         (when (< j (count sexprs))
                           1)

                         [1 1]
                         (when (and (< i (count nodes)) (< j (count sexprs)))
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
  (case (n/tag node)
    :fn       n/fn-node
    :forms    n/forms-node
    :list     n/list-node
    (condp #(%1 %2) coll
      set?    n/set-node
      map?    n/map-node
      vector? n/vector-node
      list?   n/list-node
      (throw (Exception.
               (format
                 "Not sure how to construct a node for %s (the original node had tag %s)"
                 (pr-str coll)
                 (n/tag node)))))))

(defn- rebuild-inner-node
  [node coll children]
  (if (= :deref (n/tag node))
    (n/deref-node (drop-while n/whitespace? (rest children)))
    ((inner-node-ctor node coll) children)))

(defn- needs-whitespace?
  [output-nodes new-output]
  (not
    (or (empty? output-nodes)
        (n/whitespace? (last output-nodes))
        (n/comment? (last output-nodes))
        (n/whitespace? new-output)
        (n/comment? new-output))))

(def single-space
  (n/whitespace-node " "))

(defn append-node
  [output-nodes new-output]
  (if (needs-whitespace? output-nodes new-output)
    (conj output-nodes single-space new-output)
    (conj output-nodes new-output)))

(defn rebuild-reader-fn
  [[_ snd :as form]]
  (let [syms (if (<= (count snd) 1)
               ['%]
               (map (fn [i] (symbol (str "%" i))) (range 1 (inc (count snd)))))
        replace-symbols (zipmap snd syms)]
    (last (walk/postwalk-replace replace-symbols form))))

(defn map-sort
  [node m]
  (let [node-index (->> (n/children node)
                        (remove n/printable-only?)
                        (take-nth 2)
                        (map n/sexpr)
                        (#(zipmap % (range))))]
    (->> m (sort-by #(get node-index (key %) 9999999999)) (reduce into []))))

(declare tree-update)

(defn- process-step [[output-nodes input-nodes input-sexprs] step]
  (case step
    :remove [output-nodes
             (rest input-nodes)
             input-sexprs]
    :keep   [(append-node output-nodes (first input-nodes))
             (rest input-nodes)
             input-sexprs]
    :new    [(append-node output-nodes (n/coerce (first input-sexprs)))
             input-nodes
             (rest input-sexprs)]
    :match  [(append-node output-nodes (tree-update (first input-nodes) (first input-sexprs)))
             (rest input-nodes)
             (rest input-sexprs)]))

(defn- tree-update
  [node sexprs]
  (if (and (not (n/printable-only? node))
           (= (type (n/sexpr node)) (type sexprs))
           (= (n/sexpr node) sexprs))
    node
    (match [(n/tag node) sexprs]
      [:fn (['fn* args body] :seq)]
      (tree-update node (rebuild-reader-fn sexprs))

      [:meta _] (let [data (tree-update (last (n/children node)) sexprs)]
                  (if (= (meta (n/sexpr node)) (meta sexprs))
                    (n/meta-node (first (n/children node)) data)
                    data))
      :else
      (cond
        (and (n/inner? node) (sequential? sexprs))
        (->> (find-update-plan (vec (n/children node)) (vec sexprs))
             (reduce process-step [[] (n/children node) sexprs])
             first
             (rebuild-inner-node node sexprs))

        (and (= :map (n/tag node)) (map? sexprs))
        (->> (find-update-plan (vec (n/children node)) (map-sort node sexprs))
             (reduce process-step [[] (n/children node) (map-sort node sexprs)])
             first
             (rebuild-inner-node node sexprs))

        :else
        (n/coerce sexprs)))))

(defnav SEXPRS []
  (select* [_ source-code next-fn]
    (let [tree (p/parse-string-all source-code)]
      (next-fn (n/child-sexprs tree))))
  (transform* [_ source-code next-fn]
    (let [tree (p/parse-string-all source-code)
          sexprs (vec (next-fn (n/child-sexprs tree)))
          new-tree (tree-update tree sexprs)]
      (n/string new-tree))))
