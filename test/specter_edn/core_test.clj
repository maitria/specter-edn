(ns specter-edn.core-test
  (:require [clojure.test :refer :all]
            [com.rpl.specter :refer :all]
            [com.rpl.specter.macros :refer :all]
            [specter-edn.core :refer :all]))

(deftest SEXPR-test
  (testing "can select inside stringified s-expressions"
    (are [source-code path result] (= (select path source-code) result)
      "[]"         [SEXPR]       [[]]
      "[1 2]"      [SEXPR FIRST] [1]
      ["3"]        [FIRST SEXPR] [3]
      "[1 ;??\n2]" [SEXPR LAST]  [2])))
