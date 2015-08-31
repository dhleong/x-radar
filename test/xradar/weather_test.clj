(ns xradar.weather-test
  (:require [clojure.test :refer :all]
            [xradar.weather :refer :all]))

(def lga-metar
  "KLGA 311251Z 29005KT 10SM BKN090 BKN140 BKN250 26/19 A3003")

(deftest caching-test
  (reset-weather!)
  (testing "Once received, metar is cached"
    (is (nil? (metar-for "klga")))
    (receive-metar! lga-metar)
    (is (= lga-metar (:raw (metar-for "klga"))))
    (is (= lga-metar (:raw (metar-for "KLGA")))))
  (testing "Cache can be cleared"
    (receive-metar! lga-metar)
    (is (= lga-metar (:raw (metar-for "klga"))))
    (reset-weather!)
    (is (nil? (metar-for "klga")))))

(deftest retrieve-data-test
  (testing "Winds"
    (receive-metar! "KLGA VRB05KT")
    (is (= "VRB05KT" (winds-for "klga")))
    (receive-metar! "KLGA 29005KT")
    (is (= "29005KT" (winds-for "klga")))
    (receive-metar! "KLGA 290V31005KT")
    (is (= "290V31005KT" (winds-for "klga")))
    (receive-metar! "KLGA 290V31005G09KT")
    (is (= "290V31005G09KT" (winds-for "klga")))
    (receive-metar! "KLGA 290V31005G09KT")
    (is (= "290V31005G09KT" (winds-for "klga"))))
  (testing "Altimeter"
    (receive-metar! lga-metar)
    (is (= "3003" (altimeter-for "klga"))))
  (reset-weather!))
