(ns xradar.handoff-test
  (:require [clojure.test :refer :all]
            [xradar.handoff :refer :all]))

(deftest handoff-tests
  (testing "Receive and clear"
    (let [state (atom {})]
      (on-handoff state {:from "NY_CTR"
                         :to "LGA_TWR"
                         :callsign "SAL404"
                         :cid "SAL404"})
      (is (= "NY_CTR" (clear-pending "SAL404")))
      (is (empty? @pending-handoffs)))))
