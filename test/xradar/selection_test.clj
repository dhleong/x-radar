(ns xradar.selection-test
  (:require [clojure.test :refer :all]
            [xradar.selection :refer :all]))

(deftest to-bindings-test
  (testing "Bind"
    (is (= {:j {:f {:call '(hi 1234)}}}
           (to-bindings [1234] 'hi)))))

(deftest from-bindings-test
  (testing "Unbind"
    (is (= {1234 "jf", 2345 "jd"}
           (from-bindings {:j {:f {:call '(hi 1234)}
                                      :d {:call '(hi 2345)}}})))))
