(ns xradar.selection-mode-test
  (:require [clojure.test :refer :all]
            [xradar.selection-mode :refer :all]))

(defmacro do-create-lines
  [bindings]
  `(create-lines ~'available ~'char-width
                ~'to-string ~bindings))

(deftest create-lines-test
  (testing "Single line"
    (let [available 20
          char-width 1
          to-string str]
      (is (= ["[js]Foo"] (do-create-lines
                           {"Foo" "js"})))
      (is (= ["[js]Foo  [jd]Bar"] (do-create-lines
                                    {"Foo" "js"
                                     "Bar" "jd"})))))
  (testing "Single line, barely"
    ;; just enough
    (let [available 16
          char-width 1
          to-string str]
      (is (= ["[js]Foo  [jd]Bar"] (do-create-lines
                                    {"Foo" "js"
                                     "Bar" "jd"})))))
  (testing "Multi-line"
    ;; not quite enough for a single line
    (let [available 15
          char-width 1
          to-string str]
      (is (= ["[jd]Bar" "[js]Foo"] (do-create-lines
                                     {"Foo" "js"
                                      "Bar" "jd"}))))))
