(ns specter-edn.core-test
  (:require [clojure.test :refer :all]
            [com.rpl.specter :refer :all]
            [com.rpl.specter.macros :refer :all]
            [specter-edn.core :refer :all]))

(deftest SEXPR-test
  (testing "can select inside stringified s-expressions"
    (are [structure path result] (= (select path structure) result)
      "[]"         [SEXPR]       [[]]
      "[1 2]"      [SEXPR FIRST] [1]
      ["3"]        [FIRST SEXPR] [3]
      "[1 ;??\n2]" [SEXPR LAST]  [2]))

  (testing "can transform inside stringified s-expressions"
    (are [structure path replacement result] (= (setval path replacement structure) result)
      "[1]"        [SEXPR FIRST] 2 "[2]")))
