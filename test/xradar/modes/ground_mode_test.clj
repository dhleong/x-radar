(ns xradar.modes.ground-mode-test
  (:require [clojure.test :refer :all]
            [xradar.modes.ground-mode :refer :all]))

(deftest line1-test
  (testing "Voice"
    (is (= "ACA123" (create-line1 {:callsign "ACA123"
                                   :remarks "Foo /v"}))))
  (testing "Receive"
    (is (= "ACA123/r" (create-line1 {:callsign "ACA123"
                                     :remarks "Foo /R/"}))))
  (testing "Text-only"
    (is (= "ACA123/t" (create-line1 {:callsign "ACA123"
                                     :remarks "Foo /t/"}))))
  (testing "Unknown voice capabilities"
    (is (= "ACA123/?" (create-line1 {:callsign "ACA123"
                                     :remarks "Bar"})))))

(deftest line2-test
  (testing "Incorrect squawk"
    (is (= "C178 !12" (create-line2
                        {:type "C178"
                         :squawking-mode :normal
                         :squawk "1234"
                         :squawking "1230"}))))
  (testing "Correct squawk"
    (is (= "C178 10" (create-line2
                       {:type "C178"
                        :squawking-mode :normal
                        :squawk "1234"
                        :squawking "1234"
                        :ground-speed 10}))))
  (testing "No ground speed (just in case)"
    (is (= "C178" (create-line2
                    {:type "C178"
                     :squawking-mode :normal
                     :squawk "1234"
                     :squawking "1234"})))))
