(ns xradar.commands-test
  (:require [clojure.test :refer :all]
            [xradar.commands :refer :all]))

(deftest select-aircraft-test
  (testing "Eval function"
    (let [machine {}
          state (atom {})
          result (eval-command machine state '(select-aircraft 1234))]
      (is (= 1234 (:selected @state))))))
