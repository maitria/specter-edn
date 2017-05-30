(ns specter-edn.core-test
  (:require [clojure.test :refer :all]
            [com.rpl.specter :refer :all]
            [specter-edn.core :refer :all]))

(deftest SEXPRS-test
  (testing "can select inside stringified s-expressions"
    (are [structure path result] (= (select-one path structure) result)
      "[]"         [SEXPRS FIRST]       []
      "[1 2]"      [SEXPRS FIRST FIRST] 1
      ["3"]        [FIRST SEXPRS FIRST] 3
      "[1 ;??\n2]" [SEXPRS FIRST LAST]  2
      "[]2"        [SEXPRS]             [[] 2]))

  (testing "can transform inside stringified s-expressions"
    (are [structure path replacement result] (= (setval path replacement structure)
                                                result)
      "[1]"        [SEXPRS FIRST FIRST] 2              "[2]"
      "[1] [3]"    [SEXPRS ALL FIRST]   2              "[2] [2]"
      "42; hi!\n6" [SEXPRS LAST]        9              "42; hi!\n9"
      "42; hi!\n6" [SEXPRS FIRST]       7              "7; hi!\n6"
      "[1]"        [SEXPRS]             '[(1)]         "(1)"
      "#(conj %)"  [SEXPRS FIRST LAST]  '(dissoc %)    "#(dissoc %)"))

  (testing "identity transforms preserve formatting"
    (are [structure path] (= (transform path identity structure) structure)
      "[1]"        [SEXPRS FIRST FIRST]
      "[1] [3]"    [SEXPRS ALL FIRST]
      "42; hi!\n6" [SEXPRS LAST]
      "42; hi!\n6" [SEXPRS FIRST]
      "[1]"        [SEXPRS]
      "{:a 1}"     [SEXPRS]
      "(ns specter-edn.core-test
  (:require [clojure.test :refer :all]
            [com.rpl.specter :refer :all]
            [specter-edn.core :refer :all]))"
                   [SEXPRS]))
    ;; FIXME: this sample doesn't pass
    ;"#(conj %)"  [SEXPRS])))

  (testing "constantly transforms preserve of the new structure"
      (are [structure path result] (= result (transform path (constantly '[a b c d]) structure))
        "[1]"        [SEXPRS FIRST FIRST] "[[a b c d]]"
        "[1]"        [SEXPRS] "a b c d"
        "[1] [3]"    [SEXPRS ALL FIRST] "[[a b c d]] [[a b c d]]")))

