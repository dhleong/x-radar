(ns xradar.radar-util-test
  (:require [clojure.test :refer :all]
            [xradar.radar-util :refer :all]))

(deftest update-aircraft-test
  (testing "Update Aircraft"
    (let [radar (atom {:aircraft {1234 {:cid 1234 :x 2 :y 3}}})
          updated (update-aircraft radar {:cid 1234 :foo :bar})]
      (is (= 2 (:x updated)))
      (is (= :bar (:foo updated)))))
  (testing "Location History"
    (let [radar (atom {:aircraft {1234 {:cid 1234 :x 2 :y 3}}})
          updated (update-aircraft radar {:cid 1234 :x 4 :y 5})]
      (is (= 4 (:x updated)))
      (is (= [{:x 2 :y 3}] (:history updated)))
      (let [update2 (update-aircraft radar {:cid 1234 :x 6 :y 7})]
        (is (= 6 (:x update2)))
        (is (= [{:x 4 :y 5} {:x 2 :y 3}] 
               (:history update2))))))
  (testing "First time seeing an aircraft"
    (let [radar (atom {:aircraft {}})
          updated (update-aircraft radar {:cid 1234 :x 2 :y 3})]
      (is (= 2 (:x updated)))
      ;; NB: all the above start with a nil history, so we
      ;;  know this is fine
      (is (= nil (:history updated))))))
