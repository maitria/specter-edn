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
      "[1]"           [SEXPRS FIRST FIRST]
      "[1] [3]"       [SEXPRS ALL FIRST]
      "42; hi!\n6"    [SEXPRS LAST]
      "42; hi!\n6"    [SEXPRS FIRST]
      "[1]"           [SEXPRS]
      "{:a 1}"        [SEXPRS]
      "[1 nil]"       [SEXPRS]
      "{:a 1 :b 2}"   [SEXPRS]
      "{:a #(identity %) :b 2}" [SEXPRS]
      "{:a 1
         :b 2}    "   [SEXPRS]
      "#{:a :b}"      [SEXPRS]
      "1"             [SEXPRS]
      "-1"            [SEXPRS]
      "0xff"          [SEXPRS]
      "-0xff"         [SEXPRS]
      "017"           [SEXPRS]
      "-017"          [SEXPRS]
      "2r1011"        [SEXPRS]
      "-2r1011"       [SEXPRS]
      "36rCRAZY"      [SEXPRS]
      "-36rCRAZY"     [SEXPRS]
      "7N"            [SEXPRS]
      "-5N"           [SEXPRS]
      "2/7"           [SEXPRS]
      "-22/7"         [SEXPRS]
      "1.1"           [SEXPRS]
      "-1.1"          [SEXPRS]
      "1.2e-5"        [SEXPRS]
      "-1.2e-5"       [SEXPRS]
      "1.1M"          [SEXPRS]
      "-1.1M"         [SEXPRS]
      "\"a\""         [SEXPRS]
      ":a"            [SEXPRS]
      "a"             [SEXPRS]
      "nil"           [SEXPRS]
      "true"          [SEXPRS]
      "false"         [SEXPRS]
      "(:a :b)"       [SEXPRS]
      "#(+)"          [SEXPRS]
      "#(+ %)"        [SEXPRS]
      "#(conj %1 %2)" [SEXPRS]
      "#(conj nil %)" [SEXPRS]
      "(-> a deref)"  [SEXPRS]
      "@(a x #(identity %))" [SEXPRS]
      "\"escapes \\\"\b\f\n\t octal \377 hex \ucafe\"" [SEXPRS]
      "(ns specter-edn.core-test
  (:require [clojure.test :refer :all]
            [com.rpl.specter :refer :all]
            [specter-edn.core :refer :all]))"
                   [SEXPRS]
      "(fn [x] (inc x))" [SEXPRS]))
    ;; FIXME: those samples don't pass
      ; "\r" [SEXPRS]
      ; #"pattern"        [SEXPRS]
      ; "\a" [SEXPRS]

  (testing "constantly transforms preserve of the new structure"
      (are [structure path result] (= result (transform path (constantly '[a b c d]) structure))
        "[1]"        [SEXPRS FIRST FIRST] "[[a b c d]]"
        "[1]"        [SEXPRS] "a b c d"
        "[1] [3]"    [SEXPRS ALL FIRST] "[[a b c d]] [[a b c d]]")))
