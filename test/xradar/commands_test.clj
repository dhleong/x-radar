(ns xradar.commands-test
  (:require [clojure.test :refer :all]
            [xradar.commands :refer :all]))

(deftest select-aircraft-test
  (testing "Eval function"
    (let [machine {}
          state (atom {})
          result (eval-command machine state '(select-aircraft 1234))]
      (is (= 1234 (:selected @state))))))

(deftest output-scroll-test
  (testing "Scroll by one"
    (let [machine {}
          state (atom {:output-buffer (atom [{:text "1"} {:text "2"} 
                                             {:text "3"} {:text "4"}])
                       :current-output :global
                       :output-scroll 0
                       :output-metrics-cache {:chars-per-line 100}
                       :profile {:output-size 2}})]
      (output-scroll machine state 1)
      (is (= 1 (:output-scroll @state)))
      (output-scroll machine state 1)
      (is (= 2 (:output-scroll @state)))
      ;; don't scroll past limits
      (output-scroll machine state 1)
      (is (= 2 (:output-scroll @state)))
      (output-scroll machine state -1)
      (is (= 1 (:output-scroll @state)))
      (output-scroll machine state -1)
      (is (= 0 (:output-scroll @state)))
      (output-scroll machine state -1)
      (is (= 0 (:output-scroll @state)))))
  (testing "Scroll to limits"
    (let [machine {}
          state (atom {:output-buffer (atom [{:text "1"} {:text "2"} 
                                             {:text "3"} {:text "4"}])
                       :current-output :global
                       :output-scroll 0
                       :output-metrics-cache {:chars-per-line 100}
                       :profile {:output-size 2}})]
      (output-scroll machine state 99999)
      (is (= 2 (:output-scroll @state)))
      (output-scroll machine state -99999)
      (is (= 0 (:output-scroll @state))))))
