(ns xradar.aircraft-selection-test
  (:require [clojure.test :refer :all]
            [xradar.aircraft-selection :refer :all]))

(deftest to-bindings-test
  (testing "Bind"
    (is (= {:j {:f {:call '(hi 1234)}}}
           (aircraft-to-bindings {1234 {:cid 1234}} 'hi)))))

(deftest to-aircraft-test
  (testing "Unbind"
    (is (= {1234 "jf", 2345 "jd"}
           (bindings-to-aircraft {:j {:f {:call '(hi 1234)}
                                      :d {:call '(hi 2345)}}})))))
