(ns xradar.native-insert-test
  (:require [clojure.test :refer :all]
            [xradar.native-insert :refer :all]))

(deftest select-next-var-test
  (testing "Nothing at all"
    (is (= nil (select-next-var "well, hello")))
    (is (= nil (select-next-var "well, hello" 
                                :dir :left :from 2))))
  (testing "Find the first one"
    (is (= [0 2] (select-next-var "$1, hello")))
    (is (= [7 9] (select-next-var "hello, $1"))))
  (testing "Find the next one"
    (is (= [8 10] (select-next-var "$1, via $2" :from 2)))
    (is (= [8 10] (select-next-var "hi, via $2" :from 2))))
  (testing "Loop around for next one"
    (is (= [0 2] (select-next-var "$1, via $2" :from 9))))
  (testing "Find the previous one"
    (is (= [0 2] (select-next-var "$1, via $2" 
                                   :from 8 :dir :left)))
    (is (= [8 10] (select-next-var "hi, via $2"
                                   :from 10 :dir :left))))
  (testing "Loop around for previous one"
    (is (= [8 10] (select-next-var "$1, via $2" 
                                   :from 0 :dir :left))))
  (testing "Don't be confused"
    (is (= [12 14] (select-next-var "$1, $foo at $2" :from 2)))
    (is (= [0 2] (select-next-var "$1, $foo at $2" 
                                  :from 11 :dir :left)))))
