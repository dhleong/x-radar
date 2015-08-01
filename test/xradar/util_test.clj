(ns xradar.util-test
  (:require [clojure.test :refer :all]
            [xradar.util :refer :all]))

(deftest with-alpha-test
  (testing "No alpha specified"
    (let [result (with-alpha list 0xEEEEEE)]
      (is (= 1 (count result)))))
  (testing "Alpha specified"
    (let [result (with-alpha list 0xddEEEEEE)]
      (is (= 2 (count result)))
      (is (= 0xddEEEEEE (first result)))
      (is (= 221 (second result))))))
