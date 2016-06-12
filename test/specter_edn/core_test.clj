(ns specter-edn.core-test
  (:require [clojure.test :refer :all]
            [clojure.core.unify :as u]
            [com.rpl.specter :refer :all]
            [com.rpl.specter.macros :refer :all]
            [specter-edn.core :refer :all]))

(deftest SEXPRS-test
  (testing "can select inside stringified s-expressions"
    (are [structure path pattern] (u/unify (select-one path structure) pattern)
      "[]"         [SEXPRS FIRST]       []
      "[1 2]"      [SEXPRS FIRST FIRST] 1
      ["3"]        [FIRST SEXPRS FIRST] 3
      "[1 ;??\n2]" [SEXPRS FIRST LAST]  2
      "[]2"        [SEXPRS]             [[] 2]
      "#(conj %)"  [SEXPRS FIRST]       '(fn* [?arg] (conj ?arg))
      ))

  (testing "can transform inside stringified s-expressions"
    (are [structure path replacement result] (= (setval path replacement structure)
                                                result)
      "[1]"        [SEXPRS FIRST FIRST] 2              "[2]"
      "[1] [3]"    [SEXPRS ALL FIRST]   2              "[2] [2]"
      "42; hi!\n6" [SEXPRS LAST]        9              "42; hi!\n9"
      "42; hi!\n6" [SEXPRS FIRST]       7              "7; hi!\n6"
      "[1]"        [SEXPRS]             '[(1)]         "(1)")))
