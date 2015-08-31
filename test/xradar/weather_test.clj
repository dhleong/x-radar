(ns xradar.weather-test
  (:require [clojure.test :refer :all]
            [xradar.weather :refer :all]))

(def lga-metar
  "KLGA 311251Z 29005KT 10SM BKN090 BKN140 BKN250 26/19 A3003")

(defn receive!
  [metar]
  (receive-metar! (atom {}) metar))

(deftest caching-test
  (reset-weather!)
  (testing "Once received, metar is cached"
    (is (nil? (metar-for "klga")))
    (receive! lga-metar)
    (is (= lga-metar (:raw (metar-for "klga"))))
    (is (= lga-metar (:raw (metar-for "KLGA")))))
  (testing "Cache can be cleared"
    (receive! lga-metar)
    (is (= lga-metar (:raw (metar-for "klga"))))
    (reset-weather!)
    (is (nil? (metar-for "klga")))))

(deftest retrieve-data-test
  (testing "Winds"
    (receive! "KLGA VRB05KT")
    (is (= "VRB05KT" (winds-for "klga")))
    (receive! "KLGA 29005KT")
    (is (= "29005KT" (winds-for "klga")))
    (receive! "KLGA 290V31005KT")
    (is (= "290V31005KT" (winds-for "klga")))
    (receive! "KLGA 290V31005G09KT")
    (is (= "290V31005G09KT" (winds-for "klga")))
    (receive! "KLGA 290V31005G09KT")
    (is (= "290V31005G09KT" (winds-for "klga"))))
  (testing "Altimeter"
    (receive! lga-metar)
    (is (= "3003" (altimeter-for "klga"))))
  (reset-weather!))

(deftest watching-test
  (testing "Watch"
    (is (empty? @weather-watches))
    (watch-weather! "KLGA")
    (watch-weather! "KEWR")
    (watch-weather! "KJFK")
    (is (= ["KLGA" "KEWR" "KJFK"] @weather-watches)))
  (testing "Unwatch" 
    (unwatch-weather! "KEWR")
    (is (= ["KLGA" "KJFK"] @weather-watches))
    (unwatch-weather! "KLGA")
    (is (= ["KJFK"] @weather-watches))
    (unwatch-weather! "KJFK")
    (is (empty? @weather-watches))))

(deftest ack-test
  (watch-weather! "KLGA")
  (testing "With weather reset, everything is acked"
    (reset-weather!)
    (is (true? (empty? @pending-ack)))
    (is (true? (acked? "KLGA"))))
  (testing "New metar requires ack"
    (reset-weather!)
    (receive! lga-metar)
    (is (false? (acked? "KLGA")))
    (mark-acked! "KLGA")
    (is (true? (acked? "KLGA")))
    (testing "Recieving identical metar doesn't require ack" 
      (receive! lga-metar)
      (is (true? (acked? "KLGA")))
      ;; last, receive a different metar
      (receive! "KLGA 31005KT A2992")
      (is (false? (acked? "KLGA")))
      (mark-acked! "KLGA")
      (is (true? (acked? "KLGA")))))
  (unwatch-weather! "KLGA"))
