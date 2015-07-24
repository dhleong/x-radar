(ns xradar.radar-util-test
  (:require [clojure.test :refer :all]
            [xradar.radar-util :refer :all]))

(deftest update-aircraft-test
  (testing "Update Aircraft"
    (let [radar (atom {:aircraft {1234 {:cid 1234 :x 2 :y 3}}})
          updated (update-aircraft radar {:cid 1234 :foo :bar})]
      (is (= 2 (get-in @radar [:aircraft 1234 :x])))
      (is (= :bar (get-in @radar [:aircraft 1234 :foo]))))))
