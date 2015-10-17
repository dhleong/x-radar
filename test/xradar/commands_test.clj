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

(deftest zoom-test
  (testing "set-zoom with number"
    (let [machine {}
          state (atom {:zoom 42})]
      (set-zoom machine state 10)
      (is (= 10 (:zoom @state)))
      (set-zoom machine state 9000.1)
      (is (= 9000.1 (:zoom @state)))))
  (testing "set-zoom with string"
    (let [machine {}
          state (atom {:zoom 42})]
      (set-zoom machine state "20")
      (is (= 20. (:zoom @state)))
      (let [machine 
            (set-zoom machine state "20r")]
        (is (= "Unable to set zoom to: 20r"
               (:last-echo machine))))))
  (testing "set-zoom with function"
    (let [machine {}
          state (atom {:zoom 42})]
      (set-zoom machine state #(inc (:zoom %)))
      (is (= 43 (:zoom @state)))))
  (testing "zoom-view with keywords"
    (let [machine {}
          state (atom {:zoom 42})]
      (zoom-view machine state :in)
      (is (= -8 (:zoom @state)))
      (zoom-view machine state :out)
      (is (= 42 (:zoom @state)))
      (let [machine 
            (zoom-view machine state :around)]
        (is (= "Invalid zoom direction :around"
               (:last-echo machine))))))
  (testing "zoom-view with numbers"
    (let [machine {}
          state (atom {:zoom 42})]
      (zoom-view machine state -2)
      (is (= 40 (:zoom @state)))
      (zoom-view machine state 50)
      (is (= 90 (:zoom @state))))))
