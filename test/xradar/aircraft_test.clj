(ns xradar.aircraft-test
  (:require [clojure.test :refer :all]
            [xradar.aircraft :refer :all]))

(deftest toggle-bd-test
  (testing "Toggle"
    (let [radar (atom (create-bd))]
      (is (= 0 (count @(:bd-list @radar))))
      (toggle-bd-between radar 2 3)
      (is (= 1 (count @(:bd-list @radar))))
      (toggle-bd-between radar 3 2)
      (is (= 0 (count @(:bd-list @radar)))))))
