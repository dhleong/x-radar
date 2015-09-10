(ns xradar.lists-test
  (:require [clojure.test :refer :all]
            [xradar.lists :refer :all]))

(deftest sources-test
  (testing "Arrivals"
    (is (= [{:cid 4 :arrive "KLGA"}]
           (items-for 
             {:aircraft 
              {4 {:cid 4 :arrive "KLGA"}}
              :profile
              {:arrivals #{"KLGA"}}}
             :arrivals)))
    (is (empty?
          (items-for 
            {:aircraft 
             {4 {:cid 4 :arrive "KLGA"}}
             :profile
             {:arrivals #{"KJFK"}}}
            :arrivals))))
  (testing "Departures"
    (is (= [{:cid 4 :depart "KLGA"}]
           (items-for 
             {:aircraft 
              {4 {:cid 4 :depart "KLGA"}}
              :profile
              {:departures #{"KLGA"}}}
             :departures)))
    (is (empty?
          (items-for 
            {:aircraft 
             {4 {:cid 4 :depart "KLGA"}}
             :profile
             {:departures #{"KJFK"}}}
            :departures)))))
