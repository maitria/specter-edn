(ns specter-edn.core-test
  (:require [clojure.test :refer :all]
            [com.rpl.specter :refer :all]
            [com.rpl.specter.macros :refer :all]
            [specter-edn.core :refer :all]))

(deftest SEXPRS-test
  (testing "can select inside stringified s-expressions"
    (are [structure path result] (= (select path structure) result)
      "[]"         [SEXPRS]       [[]]
      "[1 2]"      [SEXPRS FIRST] [1]
      ["3"]        [FIRST SEXPRS] [3]
      "[1 ;??\n2]" [SEXPRS LAST]  [2]
      "[]2"        [SEXPRS]       [[] 2]))

  (testing "can transform inside stringified s-expressions"
    (are [structure path replacement result] (= (setval path replacement structure)
                                                result)
      "[1]"        [SEXPRS FIRST FIRST] 2 "[2]"
      "[1] [3]"    [SEXPRS ALL FIRST] 2 "[2] [2]"
      "42; hi!\n6" [SEXPRS LAST] 9 "42; hi!\n9")))

