(ns xradar.native-insert-test
  (:require [clojure.test :refer :all]
            [xradar.native-insert :refer :all]))

(deftest select-next-var-test
  (testing "Find the first one"
    (is (= [0 2] (select-next-var "$1, hello")))
    (is (= [7 9] (select-next-var "hello, $1"))))
  (testing "Find the next one"
    (is (= [8 10] (select-next-var "$1, via $2" :from 2)))
    (is (= [8 10] (select-next-var "hi, via $2" :from 2)))))
