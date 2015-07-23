(ns xradar.aircraft-selection-test
  (:require [clojure.test :refer :all]
            [xradar.aircraft-selection :refer :all]))

(deftest to-bindings-test
  (testing "Bind"
    (is (= {:j {:f {:call '(hi 1234)}}}
           (aircraft-to-bindings {1234 {:cid 1234}} 'hi)))))
